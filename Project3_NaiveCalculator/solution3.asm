; Tested on Macos(M1) + vscode + DosboxX simulator. Untested on windows xp.
; [Outputting correctly on my mac. If things goes wrong, please contact me].

data segment 
	HIST_DX dw 0000h		; ds:[0] = DX
	HIST_AX dw 0000h 		; Used to save last result of DX:AX.

	TEMP_DX dw 0000h		; ds:[4] 
	TEMP_AX dw 0000h		; Temperary space for multiplication intermediate result.
	HEX_OUTPUT db 8 dup('0'),'h' ,'$'
	DEC_OUTPUT db 16 dup('0'), 0Ah,'$'
	PLACE_HOLDER db 0
data ends

code segment
	    assume cs: code, ds: data
	main: 
		mov ax, data
		mov ds, ax
		AND AX, 00h
		AND BX, 00h
		AND CX, 00h
		AND DX, 00h			; Init.

		MOV CH, '+'			; Init CL to ADD. 
	LOOP_INPUT:
		call Input
		MOV BL, CL
		MOV CL, CH
		MOV CH, BL 				; Pop CH (Last Operator) for calculation.
		cmp CL, '/'
		; [CHECKPOINT] Check input accuracy.
		; ======================
		;	Check: Input 1234 -> DX:AX = 0:4d2 (PASSED -> PASSED)
		;	Check: Input 5678 -> DX:AX = 0:162e (FAILED -> FIXED)
		;	Check: Input 65535 -> DX:AX = 0:ffff (PASSED)
		;	Check: Input 1234+5678* -> CH = 2B CL = * (FAILED -> PASSED) 
		;		DEBUG(FIXED): CL cleared in INPUT PROC by mistake.
		; ======================
		je ARC_DIV
		; [CHECKPOINT] Check Jump flow.
		; =====================================
		;	Check: Input 1234*5678{ENTER} -> Go to ARC_ADD -> NEXT_ARC -> END_ARC
		; =====================================
		cmp CL, '*'
		je ARC_MUL
		cmp CL, '+'
		je ARC_ADD
	NEXT_ARC:
		cmp CH, 0dh			; Check {Enter} Before going next INPUT.
		je END_ARC	
		jmp LOOP_INPUT
	ARC_ADD:
		call ADD_
		jmp NEXT_ARC
	ARC_DIV:
		call DIV_
		jmp NEXT_ARC
	ARC_MUL:
		call MUL_
		jmp NEXT_ARC
	END_ARC: 				; Input ends.
		call HtoD
		call PrintD
		MOV SI, offset HEX_OUTPUT
		call PrintH
		; MOV AH, 01h
		; int 21h				; Check OUTPUT.

		mov AX, 4c00h		; Code ends. return EXIT(0);
		int 21h

	HtoD PROC NEAR
	;==========================================================
	;	Input: 	Hex number in DX:AX.
	; 	Output: Save Decimal number in DEC_OUTPUT for later 
	;		output.
	;==========================================================
	MOV si, offset PLACE_HOLDER
	SUB si, 3		; Pointing at the last empty element in DEC_OUTPUT.
	mov DX, DS:[0]
	mov AX, DS:[2]
	HToD_NEXT:
		cmp DX, 00
		ja HToD_ABOVE
		cmp AX, 00
		je HToD_DONE		; Finish when DX: AX == 0
	HToD_ABOVE:
		mov BX, 10d
		call DIVDW
		; Remainder in BX. Actually BL.
		ADD BL, '0'
		MOV [SI], BL
		DEC SI
		jmp HToD_NEXT
	HToD_DONE:
		ret
	HtoD ENDP

	PrintH PROC NEAR
	;==========================================================
	;	Input: 	Hex number in DX:AX
	; 	Output: Print Decimal number to stdout. If output is 
	; 		less than 8 digits, 0 would be printed left to make 
	; 		up.
	;==========================================================
	; Load DX:AX
	MOV DX, DS:[0]
	MOV AX, DS:[2]

	MOV si, offset DEC_OUTPUT
	SUB si, 3		; Pointing at the last empty element in DEC_OUTPUT.
	mov DX, DS:[0]
	mov AX, DS:[2]
	PrintH_NEXT:
		cmp DX, 00
		ja PrintH_ABOVE
		cmp AX, 00
		je PrintH_DONE		; Finish when DX: AX == 0
	PrintH_ABOVE:
		mov BX, 16d
		call DIVDW
		; Remainder in BX. Actually BL.
		; [CheckPoint] if remainder in bx below 10.
		; [CHECKPOINT] If SI pointing at the end of DEC_OUTPUT.
		CMP BL, 10d
		jb PRINT_NUM
		jmp PRINT_CHAR
	PRINT_NUM:
		ADD BL, '0'
		JMP PRINTH_NUM_DONE
	PRINT_CHAR:
		SUB BL, 10d
		ADD BL, 'A'
	PRINTH_NUM_DONE:
		MOV [SI], BL
		DEC SI
		jmp PrintH_NEXT
	PrintH_DONE:
		MOV DX, offset HEX_OUTPUT
		MOV AH, 09h
		int 21h
		ret
	ret
	PrintH ENDP

	PrintD PROC NEAR
	;==========================================================
	;	Input: 	Decimal number in Mem[SI], end with '$'
	; 	Output: Print Decimal number to stdout.
	;==========================================================
		INC SI
		MOV DX, SI
		MOV AH, 09h
		int 21h
	ret
	PrintD ENDP

	MUL_ PROC near
	; =============================================
	;	Multiply number in Stack with AX
	;		Param 1: In HIST_DX:HIST_AX
	; 		Param 2: Saved in AX. 
	; 	Result is boiled down to 32 bits.
	; =============================================
		mov BX, AX
		mov AX, word ptr ds:[2]				; Pop old_AX
		mul BX	 							; Multiply AX with old AX. Result saved in Temp_{DX,AX}
		mov ds:[4], DX
		mov ds:[6], AX

		mov AX, word ptr ds:[0]				; Pop old_DX
		mul BX								; Multiply AX with old DX. 

		mov BX, word ptr ds:[4]
		add AX, BX
		mov DX, AX							; DX = (OLD_DX*AX[Low16Bits]+OLD_AX*AX[High16Bits])

		mov AX, word ptr ds:[6]				; AX = (OLD_AX*AX[Low16Bits])
	; [CHECKPOINT] CHECK Multiplication Accuracy
	; =============================================
	;	1234*65535 -> DX:AX = 04d1:fb2e 
	; 		FAILED1: 0000:0000
	; 		PASSED. 
	;	(65535+1234)*65535 -> DX:AX = (1)04cf:fb2f 
	;		FAILED2: 04d1: FB2E
	; =============================================
		MOV ds:[0], DX
		MOV ds:[2], AX
		MOV DX, 00h
		MOV AX, 00h
		ret
	MUL_ ENDP


	DIV_ PROC near
	; =============================================
	;	Divide number in Mem with AX
	;		Param 1: In HIST_DX:HIST_AX
	; 		Param 2: Saved in AX. 
	; 		Pop old_AX first and then old_DX
	; 	Result is boiled down to 32 bits. Stored in 
	;	stack. 
	; =============================================
		mov BX, AX
		; pop DX				; Pop old_AX
		mov DX, ds:[0]
		mov AX, ds:[2]	 			; Multiply AX with old AX. Result saved in Temp
		CALL DIVDW					; Result saved in DX:AX
		clc
		; AND EDX, 0000h			; Discard remainder.
		; TODO: 32 bits Div result saved to EAX. How to separate into DX:AX and PUSH into stack?
	; [CHECKPOINT] CHECK Multiplication Accuracy
	; =============================================
	;	65535 / 1234 -> DX:AX = 00:0035h
	;	65535*1234+1/1234 -> DX:AX = ? 
	; =============================================
		MOV DS:[0], DX
		MOV DS:[2], AX
		MOV DX, 00h
		MOV AX, 00h
		ret
	DIV_ ENDP

	DIVDW PROC NEAR
	;==========================================================
	;	Input: 	32 bits divident saved in DX:AX. Divisor in BX.
	; 	Output:	32 bits quotient saved in DX:AX. Remainder 
	;		in BX.
	;==========================================================
		push si
    	push cx
    	push ax

    	mov ax, dx
    	mov dx, 0  
    	div bx
    	mov si, ax
    	pop ax   
    	div bx  
    	mov bx, dx   
    	mov dx, si  

    	pop cx
    	pop si
    	ret
	DIVDW ENDP


	Input PROC near
	; ==============================================
	; 	Input: Get number from stdin.
	;		after getting from stdin:
	; 		- 	AX restores 16 bits numbers
	; 		- 	CX restores following operator. 
	; 				42d for * 43d for + 47d for /.
	;		if no following operator. CL = 0dh.
	; ==============================================
		push BX	; BX used to save mul number.
		push DX ; DX used to save temperary read-in number.
		and BH, 00h	
		mov BL, 10d	

		and CL, 00h       ; Clear CL. CH not changes.
	input_next:
		mov AH, 1			; AX used for mul each time. 
		int 21h
		; cmp CL, 0dh       ; Judge if enter.
		; je  input_done
		cmp AL, 30h
		jb input_done
	is_number:
		and ax, 00FFh
		sub ax, '0'		; Convert ascii code to number.

		push DX
		push AX
		pop SI			; SI is used to save old_DX temprarily.
		pop AX		

		mul BX			; BX * AX -> 00:AX 
		mov DX, SI
		add DX, AX
		jmp input_next
		; [CHECKPOINT] Check if each digit cause right value in DX
		; ==============================================
		;	5678 -> 5 -> 38 -> 237 (PASSED) -> 162e (FAILED -> FIXED).
		;	DEBUG(FIXED): MUL BX but not BL.
		; ==============================================
	input_done:
		mov CL, AL ; Save Operator to CX. [CHECKPOINT] 
		mov AX, DX

		pop DX
		pop BX
		ret
	Input ENDP

	ADD_ PROC near
	; =============================================
	;	Adding number in AX to Stack.
	;		Param 1: In HIST_DX:HIST_AX
	; 		Param 2: Saved in reg AX
	; 	Result is boiled down to 32 bits. Stored in 
	;	HIST_DX:HIST_AX. 
	; =============================================
		mov DX, word ptr ds:[2]		; Pop old_AX
		add AX, DX 					; Add AX and Old_AX
		mov DX, word ptr ds:[0]		; Pop old_DX
		adc DX, 00h			; ADD carry to OLD_DS
	; [CHECKPOINT] CHECK Adding Accuracy
	; =============================================
	;	1234+65535 -> DX:AX = 0001:04d1 
	; 		FAILED1: 0001: 002e
	; 		DEBUGGED.
	; =============================================
		clc
		MOV DS:[0], DX
		MOV DS:[2], AX
		MOV DX, 00h
		MOV AX, 00h
		ret
	ADD_ ENDP


code ends
end main