; Tested on Macos(M1) + vscode + DosboxX simulator. Untested on windows xp.
; Test goes all right on my machine.
code segment
	    assume cs: code
	main:       
		mov ax, 0b800h              ; Initialize EX
		mov es, ax

	clear:                          ; Clear the display.
		mov di, 0h
	clear_loop:
		mov byte ptr es:[di], 00h
		inc di
		cmp di, 0fa0h
		jne clear_loop

		mov di, 1*160+0             ; Start Position
		mov si, 00h

	row_loop:
		PUSH di
		PUSH si						; Push stack to save di & si as [Start Position in eachline] and [Start ASCII code for the first character].
	inline_loop:
		mov dx, si
		call Print
		add di, 10					; 5 chars interval for adjacent columns.
		add si, 19h					; mov on to the next column
		cmp si, 256D
		jb inline_loop
		POP si
		POP di
	
		add di, 160d
		add si, 1d
		cmp si, 0019h
		jb row_loop

		mov ax, 4c00h
		int 21h

	toHex PROC NEAR
	;==========================================================
	;	Input: 	Decimal number in dl. Range from 0 to 255
	; 	Output: Hex number in BX. [BH][BL]H
	;==========================================================
			mov bh, dl
			push cx
			mov cl, 4
			shr bh, cl
			pop cx
			cmp bh, 09D
			jbe number_h
			jmp char_h
		number_h: 				; Output ASCII code depending on type.
			add bh, 30h
			jmp next_h
		char_h:
			add bh, 55D
		next_h:
			mov bl, dl
			and bl, 00001111B
			cmp bl, 09D
			jbe number_l
			jmp char_l
		number_l: 
			add bl, 30h
			ret
		char_l:
			add bl, 55D
			ret
	toHex ENDP
	
	Print PROC near
	; ==============================================
	; 	Print: Ascii Sign and its number
	;		Position: di
	;       Char: 	  dl
	;   After call di changes.
	; ==============================================
		PUSH DX
		mov dh,00000100B              ; Print [dl] in color [dh]
		mov es:[di],dx
		add di, 2

		call toHex

		mov dh, 00000010B
		mov dl, bh
		mov es:[di],dx
		add di, 2

		mov dh, 00000010B
		mov dl, bl
		mov es:[di],dx
		POP DX

		ret
	Print ENDP

code ends
end main