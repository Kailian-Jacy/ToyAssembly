; Tested on Macos(M1) + vscode + DosboxX simulator. Untested on windows xp.
; [Outputting correctly on my mac. If something goes wrong, please contact me: 3200100963].

data segment
	; Prompt messages.
	Prompt db "Please input filename:", 0Ah, "$"
	Error_OpenFailed db 0Ah, "Cannot open file!" ,"$"
	Error_KeyStrokeMissed db 0Ah, "Keystroke input is not valid!", "$"

	; File related metadata.
	Filename db 64h, 101 dup(0)	; Filename size limited.   int 21h 03d expecting a Zero-ENDED filename.
	; Filename db "D:\TESTTXT", 0	; Filename with test file given.
	File_Handler dw 0000h
	File_Size db 4 dup('0')

    ; Page 
	Buffer db 256 dup(0)			; Buffer for reading file.
	Byte_in_buf dw 00h
	Offsets db 4 dup(0)             ; Offset called by `show_this_page`
        ; offsets[0] dw saves the lower 16 bits address: Offset.
        ; offsets[2] dw saves the higher 16 bits address: segment.

    ; Row
    rows db 0d
    Byte_on_row db 0d
    Buffer_On_Row db 20 dup(0)                ; Up to 16 Bytes makes a row.
    Video_Offset dw 0d
    location_seg dw 0000h
    location_off dw 0000h ; 32 bits location

    ; Displayed string 
    template_S db "00000000:            |           |           |                             $$$$"
    conv_table db "0123456789ABCDEF$"
    end_pattern db 0

    ; F5 keystroke temp
    Offset_temp db 4 dup(0)
    input_chars db 0
    box_buf db 72 dup(0)
    box_input db 9 dup(0)
    box db "+----------+|          |+----------+$"
data ends

code segment
	assume cs: code, ds: data

OpenFile_ PROC near
; =======================================
;	Get Input and Open file. 
;	Halting program if failed.
; =======================================
	; Print message
	push dx
	; Debug Log: Unmatched pop would cause return error.

; Commented for reading-in test file,
	mov dx, offset Prompt
	mov ah, 09h
	int 21h
	; Get FilenameInput
	mov dx, offset Filename
	mov ah, 0Ah
	int 21h
	; DEBUGG: Trailing 0Dh input with ENTER.
	push di
	mov di, dx
	mov di, [di+1]
	and di, 00FFh
	add di, dx
	mov byte ptr [di+2], 00h
	pop di

	; Openfile
	mov ah, 3Dh
	mov al, 0
	mov dx, offset Filename
	add dx, 02h
	int 21h; CF=0 on success, AX=handle
	; Check if succeeded.
	mov dx, 00h
	adc dx, 00h
	cmp dx, 01h
	je Error_OpenFailed_
	; Load Handler if succeeded.
	mov File_Handler, ax
	pop dx
	ret
OpenFile_ ENDP

Error_OpenFailed_:
	mov dx, offset Error_OpenFailed
	mov ah, 09h
	int 21h
	mov ah, 0Ch
	int 21h

main: 
	mov ax, data
	mov ds, ax 

    mov ax, 0B800h
    mov es, ax

	; Open given file and init file handler.
	call OpenFile_
	push di
	mov di, offset File_Handler
	pop di
	; Save the size to file_Size and rewind file offset.
	call GetLength_	
	push di
	mov di, offset File_Size
	pop di

main_loop:
; =======================================
;    Main loop for Reading and displaying
;		AX: Key Input. Check for quitting.
; =======================================
	call GetBytesInBuf_
	push di
	mov di, offset Byte_in_buf
	pop di
	; =======================================
	; [CheckPoint] Check bytes on buf
	; =======================================
	call Fread_
	push di
	mov di, offset buffer
	pop di
	call ShowPage_

	; Get Keystroke input
	mov ah, 0
	int 16h

	cmp ah, 47h   
    je call_home
	cmp ah, 49h
    je call_PageUp
	cmp ah, 51h
    je call_PageDown
	cmp ah, 4fh
    je call_End
	cmp ah, 3fh
    je call_F5

call_Home:
    ; [TESTED] Home
	call Home_
	jmp break;

call_PageUp:
    ; [TESTED] PageUp
	call PageUp_
	jmp break;
call_PageDown:
    ; [TESTED] PageDown
	call PageDown_
	jmp break;
call_End:
    ; [TESTED] End
	call End_
	jmp break;

call_F5:
	call F5_
	jmp break;

; 	cmp al, 4900d
; 	; Most complex... Write it later.
; 	call F5_
; 	jmp break;

break:
; 	; Judge to break and close
; 	cmp al, 4900d
; 	jmp ending

; 	jmp Error_KeyStrokeMissed_
    jmp main_loop

ending:
	; Close(fp)
	mov ah, 3Eh
	mov bx, File_Handler
	int 21h
	; EXIT(0)
	mov ax, 4C00h
	int 21h

DisplayString_ PROC
; =======================================
; Print given disposed string on screen
;   Input: template_s
;       VideoOffset
; =======================================
    PUSH SI
    PUSH DI
    PUSH BX
    PUSH AX
    PUSH cx

    mov di, offset template_S     ; The string to be inserted.
    mov dx, di
    add dx, 59d                     ; The last possible highlight |
    mov si, offset video_offset   ; The video offset in 0xB800ABCD.
    mov si , [si]

    mov cx, 75d
DisplayString_Loop:
    mov al, ds:[di]
    mov es:[si], al
    inc si

    ; offset for 59d
    cmp di, dx
    jae NotPipeSign
    cmp al, '|'
    jne NotPipeSign
    mov ah, 0Fh
    jmp DisplayString_Pipe_JudgeEnd
NotPipeSign:
    mov ah, 07h
DisplayString_Pipe_JudgeEnd:
    mov es:[si], ah

    inc si
    inc di
    loop DisplayString_Loop

    ; Add For Test Purpose
    ; mov ax, 0100h
    ; int 21h

	POP cx
    POP AX
    POP BX
    POP DI
    POP SI
    ret
DisplayString_ ENDP

Home_ Proc near
; =======================================
;    Keystroke processing proc.
; 		Rewinding offsets.
; 		offset = 0;
; =======================================
	push si
	push bx
	mov si, offset offsets
	mov word ptr ds:[si], 0000h
	mov word ptr ds:[si+2], 0000h
    ; =======================================
    ; [CheckPoint] Check offsets changes. [PASSED]
    ; =======================================
	pop bx
	pop si
	ret
Home_ ENDP

End_ Proc near
; =======================================
;    Keystroke processing proc.
; 		Get proer offsets to show last page.
; =======================================
	push si
	push dx
    push ax
    push bx

    ; filesize % 256 == 0?
	mov si, offset File_Size
    ; =======================================
    ; [CheckPoint] Check FileSize.
    ; Test1: fs % 256 != 0; 
    ; =======================================
	mov ax, ds:[si]
	mov dx, ds:[si+2]       ; dx:ax = File_Size
    mov bx, 256d

    div bx
    cmp dx, 0d              
    je End_Divisable

    ; if file % 256 <> 0
    mov bx, dx              ; remainder saved to bx.
	mov ax, ds:[si]
	mov dx, ds:[si+2]

    sub ax, bx              ; remainder < 256, it must be contained in ax.
    sbb dx, 00d
    mov si, offset offsets
    mov [si], ax 
    mov [si+2], dx          ; Save offsets.
    ; =======================================
    ; [CheckPoint] Check correspondent result.
    ; Test1: offsets = filesize - 256; Fs unchanged.
    ; =======================================
    jmp End_Ending

    ; if file % 256 == 0
End_Divisable:
	mov si, offset File_Size
	mov ax, ds:[si]
	mov dx, ds:[si+2]       ; dx:ax = File_Size

    sub ax, 256d
    sbb dx, 00d
    mov si, offset offsets
    ; =======================================
    ; [CheckPoint] Check correspondent result.
    ; Test2: offsets = filesize - 256; Fs unchanged.
    ; =======================================
    mov [si], ax 
    mov [si+2], dx           ; Save offsets.

End_Ending:
    pop bx
    pop ax
	pop dx
	pop si
	ret
End_ ENDP

PageUp_ PROC NEAR
; =======================================
;    Keystroke processing proc.
; =======================================
    push di
    push ax 
    push bx

    mov di, offset offsets[0]
    mov ax, [di]

    cmp ax, 256d
    mov bx, [di+2]          ; bx:ax = offsets.
    jae PageUp_Bigger
    cmp bx, 0d
    jne PageUp_Bigger

    call Home_              ; Clear offsets
    jmp PageUp_Ending
PageUp_Bigger:
    sub ax, 256d
    sbb bx, 0d
    mov [di], ax
    mov [di+2], bx

PageUp_Ending:
    pop bx
    pop ax
    pop di
    ret
PageUp_ ENDP

PageDown_ PROC NEAR
; =======================================
;    Keystroke processing proc.
; 		Get down one page.
;           offset = offset + 256;
; =======================================
    push di
    push ax 
    push bx     ; ax:bx saves old offsets
    push cx
    push dx     ; dx:dx saves file_size

    mov di, offset offsets[0]
    mov ax, [di+2]
    mov bx, [di]
    add bx, 256d
    adc ax, 00d         ; ax:bx = offset +  256d

    mov di, offset File_Size[0]
    mov cx, [di+2]
    mov dx, [di]        ; cx:dx = File_Size

    sub bx, dx
    sbb ax, cx          ; offset + 256 - file_size

    jc PageDown_Smaller
    jmp PageDown_Ending

PageDown_Smaller:
    ; save offsets
    mov di, offset offsets[0]
    mov ax, [di+2]
    mov bx, [di]
    add bx, 256d
    adc ax, 00d         ; ax:bx = offset + 256d
    mov [di+2], ax
    mov [di], bx
; =======================================
; [CheckPoint] Check offsets change.
; =======================================

PageDown_Ending:
    pop dx
    pop cx
    pop bx
    pop ax
    pop di
    ret
PageDown_ ENDP

F5_ PROC near
; =======================================
;   Keystroke processing proc.
; 		GetInput from screen and go there.
;   offset_temp = get_offset()
;   if offset_temp < filesize
;   offset = new offset
; =======================================
    call Get_Offset_Input_
    call CheckInput_
    ret
F5_ ENDP


CheckInput_ PROC near
; =======================================
;   Check user input in temp_offset, and
;   decide if to replace the old one.
;       In/Output: Check input
; =======================================  
    ; Check input validity
    PUSH DI
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

    mov di, offset offset_temp
    mov ax, [di]
    mov dx, [di+2]      ; DX:AX = temp_offset

    mov di, offset File_Size
    mov bx, [di]
    mov cx, [di+2]      ; CX:BX = FileSize

    sub bx, ax
    sbb cx, dx          ; File_Size - temp_offsets

    ; Do not replace if temp_offsets is bigger
    jc Check_Input_Ending
    cmp cx, 00d
    jne Check_Input_Replace
    cmp bx, 00d
    je Check_Input_Ending   ; Not switching if equal.
Check_Input_Replace:
    ; Replace if temp_offset is smaller (Sub results to positive number)
    ; Save DX:AX -> offsets.
    mov di, offset offsets
    mov [di], ax
    mov [di+2], dx

Check_Input_Ending:
    POP DX
    POP CX
    POP BX
    POP AX
    POP DI
    ret
CheckInput_ ENDP


Enter_ PROC near
; =======================================
;    Disposal: Input an '\0' at the end.
; =======================================
    push di
    push si
    mov di, offset input_chars
    mov si, [di]            ; si = n
    and si, 00ffh

    mov di, offset box_input
    add di, si              ; box_input[n]

    mov byte ptr [di], 0d
    pop si
    pop di
    ret
Enter_ ENDP

BKSpace_ PROC near
; =======================================
;    Keystroke disposal progress.
;       Delete char.
;    Input: DI (offset P) & Input_chars
; =======================================
    push si
    push di
    mov di, offset input_chars
    mov si, [di]            ; si = input chars
    and si, 00ffh
    pop di

    cmp si, 00d
    je BKSpace_End

    ; Point DI at the current position.
    mov di, 1990d

    ; si = (n-1) * 2
    dec si
    shl si, 1d
    add si, di      ; p[(n-1) * 2]
    mov byte ptr es:[si], ' '

    inc si
    mov byte ptr es:[si], 17h

    ; n--
    push di
    mov di, offset input_chars
    mov si, [di]            ; si = input chars
    dec si
    mov [di], si
    ; =======================================
    ; [CheckPoint] Check input_chars & screen.
    ; =======================================
    pop di
BKSpace_End:
    pop si
    ret
BKSpace_ ENDP

Display_Box_ PROC NEAR
; =======================================
;    DisplayBox and get user input 
;       Output: box_input (9 Byte)
;       Unchecked ASCII code.
; =======================================
    ; PROTECTED: DI. used as p.
    push di 
    push si     
    push cx
    PUSH AX
    mov di, 1828d           ; p = vp + (y * 80 + x) * 2; offset p = 1828
    mov si, offset box_buf

    ; Save to buf
    mov cx, 3d
; =======================================
; [CheckPoint] Check Saved Content.
; =======================================
Display_Box_Save_Loop:
    PUSH CX
    mov cx, 24d
Display_Box_Save_Inner_Loop:
    mov al, es:[di]
    mov [si], al
    inc si
    inc di
    loop Display_Box_Save_Inner_Loop 
    add di, 136d
    POP CX
    loop Display_Box_Save_Loop

    ; output box
    mov di, 1828d
    mov si, offset box
    mov cx, 3d
Display_Box_Loop:
    PUSH CX
    mov cx, 12d
Display_Box_Inner_Loop:
    mov al, [si]
    mov es:[di], al
    inc di
    mov byte ptr es:[di], 17h
    inc di
    inc si
    loop Display_Box_Inner_Loop 
    add di, 136d
    POP CX
    ; For test purpose.
    ; mov ah, 01h
    ; int 21h
    loop Display_Box_Loop

    ; get stroke
    call GetInputKey_

    ; For test purpose.
    ; mov ah, 01h
    ; int 21h

    ; Recover screen display
    mov cx, 3d
    mov di, 1828d   ; Pointing at vp.
    mov si, offset box_buf
; =======================================
; [CheckPoint] Check Saved Content.
; =======================================
Display_Box_Recover_Loop:
    PUSH CX
    mov cx, 24d
Display_Box_Recover_Inner_Loop:
    mov al, [si]
    mov es:[di], al
    inc si
    inc di
    loop Display_Box_Recover_Inner_Loop 
    add di, 136d
    POP CX
    loop Display_Box_Recover_Loop

    pop ax
	pop cx
    pop si
    pop di
    ret
Display_Box_ ENDP

GetInputKey_ PROC near
    PUSH DI ; Protected DI, used to save offset p
    PUSH SI
    PUSH AX
    PUSH CX 

    mov di, offset input_chars
    mov byte ptr [di], 0d    ; Clear length

	; p = vp + ((y + 1) * 80 + (x + 1)) * 2; offset p = (12 * 80 + 35) * 2
    mov di, 1990d
GetInputKey_Loop:
	; Get Keystroke input
	mov ah, 0
	int 16h

    ; =======================================
    ; [CheckPoint] Check Input key
    ; Test1: Enter Expect: ah=1C [PASSED]
    ; Test2: f -> Get 2166
    ; =======================================

    cmp ah, 0Eh
    je Call_BKSpace
    cmp ah, 1Ch
    je Call_Enter
    jmp GetInputKey_Break
Call_BKSpace:
    ; [TESTED] BKSpace.
    Call BKSpace_
    jmp GetInputKey_Break
Call_Enter:
    ; [TESTED] Enter.
    Call Enter_ 
    jmp GetInputKey_Ending         ; End input.
GetInputKey_Break:
    ; Check Input validity

    ; Convert Upper class
    cmp al, 'a'
    jb GetInputKey_NotLowerCase
    cmp al, 'f'
    ja GetInputKey_NotLowerCase
    ; sub ah, 'a'-'A'
    sub al, 20h
GetInputKey_NotLowerCase:
    ; Now keys are all upper case.
    ; =======================================
    ; [CheckPoint] Check input upper case
    ; Test1: lower case. [PASSED]
    ; Test1: number.    [PASSED]
    ; =======================================

    ; Check if in '0-9A-F'
    mov si, offset conv_table
    ; =======================================
    ; [CheckPoint] 
    ; Test1: 2 -> correct
    ; Test2: f -> ? [PASSED]
    ; Test3: g -> ? [PASSED]
    ; =======================================
GetInputKey_InTable_Loop:
    cmp al, [si]
    je GetInputKey_InTable
    inc si
    cmp si, offset conv_table + 16d
    jbe GetInputKey_InTable_Loop

    ; Not In Table
    jmp GetInputKey_Loop

GetInputKey_InTable:
    ; Check length
    mov si, offset input_chars
    mov cl, [si]    ; cl = n
    mov ch, 00d

    ; More than 8 numbers
    ; =======================================
    ; [CheckPoint] Check Input 8 chars
    ; =======================================
    cmp cl, 8d
    jae GetInputKey_Loop

    ; Display this char
    push si
    mov si, offset box_input
    add si, cx
    mov [si], al        ; Input[n] = key
    ; =======================================
    ; [CheckPoint] Check key saved.
    ;   [PASSED]
    ; =======================================
    pop si

    push di
    push cx
    shl cl, 1d       ; cl = n * 2
    add di, cx      ; di = p[2n]
    mov es:[di], al
    ; =======================================
    ; [CheckPoint] Check di location
    ; Test1: di should be pointing at final 
    ;   displayed char.
    ; =======================================
    inc di
    mov byte ptr es:[di], 17h
    pop cx
    pop di

    ; Update n
    inc cl
    mov si, offset input_chars
    mov [si], cl
    ; =======================================
    ; [CheckPoint] If Cl written in properly
    ; =======================================
    
    ; Only ending the loop when {Enter} is detected.
    jmp GetInputKey_Loop

GetInputKey_Ending:
    pop cx
    pop ax
    pop SI
    POP DI
    ret
GetInputKey_ ENDP

ToNumber_ PROC near
; =======================================
;  Convert input get from user to hex.
;   Input: box_input (9 Bytes)
;   Output: temp_offset (4 Bytes)
; =======================================
    PUSH di
    PUSH si
    PUSH CX
    mov di, offset box_input
    mov cx, 0d
    ; =======================================
    ; [CheckPoint] Check box_input validity.
    ;   [PASSED]
    ; =======================================
ToNumber_Traverse_Loop:
    mov si, di
    add si, cx
    mov ah, 00h
    mov al, [si]                ; Get box_input[cx]
    ; End converting when reading \0
    cmp al, 0d
    je ToNumber_Ending          

    ; Traverse to get real number
    Call GetRealNumber_
    ; temp_offset move left 4 bit
    Call LeftMove_

    ; Save al to the tail of temp_offset.
    push di
    push cx
    mov di, offset offset_temp
    mov cl, [di]                ; Least byte.
    and cl, 0f0h
    and al, 00fh
    add cl, al
    mov [di], cl                ; Save to the last byte.
    ; =======================================
    ; [CheckPoint] Check last byte.
    ; =======================================
    pop cx
    pop di

    inc cx
    jmp ToNumber_Traverse_Loop
ToNumber_Ending:
    POP CX
    POP si
    POP di
    ret
ToNumber_ ENDP

LeftMove_ PROC near
; =======================================
;    Left move 4 bits of temp_offset
; =======================================
    PUSH Di
    push ax
    push dx
    push cx             ; Cx is used to stash the highest 4 bit of ax.
    
    mov cx, 00h         ; clear cx

    mov di, offset offset_temp
    mov dx, [di+2]
    mov ax, [di]

    mov cx, ax
    and cx, 0f000h

    push ax
    mov ax, cx
    mov cl, 12d
    shr ax, cl
    mov cx, ax
    pop ax
    ; =======================================
    ; [CheckPoint] Check cx. Only one bit from ax.
    ; =======================================
    
    push cx
    mov cl, 4d
    shl ax, cl
    shl bx, cl
    pop cx
    add bx, cx

    ; Save to temp_offset.
    mov di, offset offset_temp
    mov [di+2], dx
    mov [di], ax

    pop cx
    pop dx
    pop ax
    pop di
    ret
LeftMove_ ENDP


GetRealNumber_ PROC near
; =======================================
;    Input: al as char to search.
;    Output: al as the true value
; =======================================
    push bx
    push si
    push di

    mov ah, 0d
    mov di, offset conv_table
    mov bx, 0d
GetRealNumber_Loop:
    mov dl, [di]
    cmp dl, al
    je GetRealNumber_Equal
	inc di
    inc ah
    jmp GetRealNumber_Loop ; No need to check border again. It must skip out somewhere in the middle.
GetRealNumber_Equal:
    ; Save the true value in al.
    mov al, ah

    pop di
    pop si
    pop bx
    ret
GetRealNumber_ ENDP

Get_Offset_Input_ PROC near
; =======================================
;  get_offset(). 
;   Output: offset_temp updated, unchecked.
; =======================================
    push di
    push si
    push cx

    ; Clear old offset_temp
    mov di, offset offset_temp
    mov cx, 2d
Get_Offset_Input_Clear_Loop:
    mov word ptr [di], 0000h
    add di, 2d
    loop Get_Offset_Input_Clear_Loop

    ; Display box and Get from user
    Call Display_Box_

    ; Convert ASCII input to number offset.
    push di
    mov di, offset box_input
    pop di
    push di
    mov di, offset offset_temp
    pop di
    Call ToNumber_
    pop cx
    pop si
    pop di
    ret
Get_Offset_Input_ ENDP


ShowPage_ PROC near
; =======================================
;    Print the page in buffer.
;		Input: 
; 			- Buffer & bytes_in-buf
;			- Offsets(4B).
; =======================================
    call ClearThisPage_
    call GetRow_

    push si
    push di
    mov di, offset rows
    mov di, [di]            ; di restores the total row number. It's fixed during the loop.
    and di, 00ffh
    mov si, 0               ; si is the iteratve index. 

    ; =======================================
    ; [CheckPoint] Check DI, SI
    ; =======================================
show_page_loop:
    ; PROTECTED: SI DI (Looper)
    ; =======================================
    ; [CheckPoint] Check buffer offset and 
    ;   param when moving to next row.
    ; =======================================
    PUSH DI
    mov di, offset byte_on_row
    POP DI
    call GetBytesOnRow_
    ; =======================================
    ; [CheckPoint] Check byte_on_row
    ;   Row = 1, Expect to be 10h [PASSED]
    ; =======================================
    PUSH DI
    mov di, offset location_seg
    POP DI
    call GetOffsetOfRow_
    ; =======================================
    ; [CheckPoint] Check Offset_Of_row
    ;   Row = 1, Expect to be 0000h, 0000h [PASSED]
    ;   Row = 2, Expect to be 0000h, 1000h [PASSED]
    ; =======================================
    PUSH DI
    mov di, offset buffer_on_row
    POP DI
    call GetBufferOnRow_
    PUSH DI
    mov di, offset template_S
    POP DI
    call GetPointer_
    call CharToHex_
    call CharToAscii_
    ; =======================================
    ; [CheckPoint] Check template_S
    ;   Row = 1, Expect to be ...
    ; =======================================
    PUSH DI
    mov di, offset video_offset
    POP DI
    call SetVideoLocation_
    ; =======================================
    ; [CheckPoint] Check Video_offset
    ;   Row = 1, Expect to be 00h
    ; =======================================
    call DisplayString_
    ; Add For Test Purpose
    ; mov ax, 0100h
    ; int 21h

    inc si
    cmp si, di
    jne show_page_loop
    
    pop si
    pop di

    ; Continue to manupate the key in hw4.asm
    mov ax, 0C00h
    int 21h
; DEBUG in another file.
	ret
ShowPage_ ENDP

GetBufferOnRow_ PROC near
; =======================================
; Copy buffer for row from total buffer
;   Input: Buffer & Byte_on_row
;   Output: Buffer_on_row
; =======================================
    PUSH di
    PUSH si
    PUSH CX
    PUSH AX

	PUSH si
    ; Clear this buffer
    mov cl, 20d
    mov ch, 00h
    mov si, offset buffer_on_row
    mov al, 00d
GetBufferOnRow_Clear_Loop:
    mov [si], al
    inc si
    loop GetBufferOnRow_Clear_Loop
    POP SI

    mov di, offset byte_on_row
    mov cl, [di]
    mov ch, 00h
    mov di, offset buffer
    push cx
    mov cl, 4d
    shl si, cl                  ; i *= 16
    pop cx
    add di, si                  ; di = buffer[i*16]
    mov si, offset buffer_on_row
GetBufferOnRow_Loop:
    mov al, [di]
    mov [si], al
    inc di
    inc si
    loop GetBufferOnRow_Loop

    mov di, offset buffer_on_row
    ; =======================================
    ; [CheckPoint] Check buffer_on_row.
    ; =======================================
    POP AX
    POP CX
    POP SI
    POP DI
    ret
GetBufferOnRow_ ENDP

GetBytesInBuf_ Proc near
; =======================================
;    Decide how many chars to read in.
;		- Offset + BufInSize < File_Size
;		- BufSize < 256
; =======================================
	push cx
	push bx
	push ax
	push dx
	; n = File_Size - Offset. n = cx:bx
	push di
	mov di, offset File_Size[0]
	mov bx, [di] 
	mov di, offset offsets[0]
	mov ax, [di]
	sub bx, ax
	mov di, offset File_Size[2]
	mov cx, [di]
	mov di, offset offsets[2]
	mov ax, [di]
	sbb cx, ax
	pop di
	
	; CX>1 or bx>256d
	cmp cx, 0
	ja over256
	cmp bx, 256d
	jae over256

; Less256:
	push di
	mov di, offset Byte_in_buf
	mov [di], bx
	pop di
	jmp GetBytes_ending

over256:
	push di
	mov di, offset Byte_in_buf
	mov word ptr [di], 256d
	pop di
	jmp GetBytes_ending

GetBytes_ending:
	pop dx
	pop ax
	pop bx
	pop cx
	ret
GetBytesInBuf_ ENDP


Fread_ PROC near
; =======================================
; Read from Opened file. 
;		Input: Bytes_In_buf & offsets (4B).
;		Output: Buffer.
; =======================================
	push bx
	push cx
	push dx

	; Set fp with offset
	mov ax, 4200h
	mov bx, File_Handler
	mov cx, word ptr offsets[2]; \cx:dx一起构成
	mov dx, word ptr offsets[0]; /32位值=offset
	int 21h
	
	; Read from file.
	; bytes_in_buf = fread(buf, 1, bytes_in_buf, fp);
	mov bx, File_Handler
	push di
	mov di, offset Byte_in_buf
	mov cx, [di]
	pop di

	mov dx, offset Buffer; ds:dx->buf
	mov ax, seg Buffer
	mov ds, ax

	mov ax, 3F00h
	int 21h; CF=0 on success, AX=bytes actually read
	; =======================================
	; [CheckPoint] AX equals to CX?
	; 	- AX = CX.
	;	- CF = 0.
	; =======================================

	pop dx
	pop cx
	pop bx
	ret
Fread_ ENDP

Error_KeyStrokeMissed_:
	mov dx, offset Error_KeyStrokeMissed
	mov ah, 09h
	int 21h
	mov ah, 0Ch
	int 21h

GetLength_ PROC near
; =======================================
;    Get file length and rewind.
;		Input:  File_Handler in memory.
;		Output: File_Size(dw)
; =======================================
	push bx
	push dx
	push cx

	; Move to EOF to get length.
	mov bx, File_Handler
	mov cx, 0 
	mov dx, 0
	mov ah, 42h
	mov al, 2
	int 21h  
	mov word ptr File_Size[2], dx
	mov word ptr File_Size[0], ax

	; mov dx, offset File_Size
	; =======================================
	; [CheckPoint]
	; Test1[Passed]: Check value of File_Size
	; =======================================

	; Rewind to beginning
	mov bx, File_Handler
	mov cx, 0;\ 移动距离 = 0
	mov dx, 0;/
	mov ah, 42h
	mov al, 0; SEEK_SET, 以文件内容的首字节为起点移动文件指针
	int 21h

	pop bx
	pop dx
	pop cx
	ret
GetLength_ ENDP


SetVideoLocation_ PROC near
; =======================================
;    Set video address:
;       vp = 0xB8000000 + row * 80 * 2;				
;   Input: SI (Row)
; =======================================
    PUSH DX 
    PUSH AX
    PUSH di
    
    ; mov di, offset cur_row
    ; mov al, [di]
    mov ax, si
    mov dl, 160d
    mul dl

    mov di, offset video_offset
    mov [di], ax
    ; =======================================
    ; [CheckPoint] Check video_offset
    ;   Test1:  CurRow=15 
    ;   Expect: Video_address=960 [PASSED]
    ; =======================================

    pop di
    POP AX
    POP DX
    ret
SetVideoLocation_ ENDP

CharToAscii_ PROC near
; =======================================
;    Copy buffer_on_row for display.
; =======================================
    PUSH di
    PUSH si
    PUSH ax
    PUSH BX

    ; mov di, offset byte_on_row 
    ; mov cx, [di]; Content the length
    ; and cx, 00ffh
    mov cx, 16d

    mov si, offset buffer_on_row
    mov di, offset template_S
    add di, 59d
    ; =======================================
    ; [CheckPoint] Check if it's removing the
    ;   first dot. [PASSED]
    ; =======================================
CharToAscii_Loop:
    mov bl, [si]
    mov [di], bl
    inc si
    inc di
    loop CharToAscii_Loop
    ; =======================================
    ; [CheckPoint] Check Full copy. [PASSED]
    ; =======================================

	pop bx
    POP AX
    POP si
    POP di
    ret
CharToAscii_ ENDP

CharToHex_ PROC near
; =======================================
; Convert Chars in buffer to hex.
;    Input: Bytes_on_row; Buffer_On_Row.
;    Output: template_S
; =======================================
    PUSH di
    PUSH si
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

    ; mov di, offset byte_on_row  
    ; mov cx, [di]
    ; and cx, 00ffh   ; cx: The length of row
    mov cx, 16d
    ; =======================================
    ; [CheckPoint] Check cx [PASSED]
    ; =======================================

    mov si, 0d          ; si points at the char in buffer.
    mov bx, offset conv_table
    mov di, offset template_S
    add di, 10d         ; di points at the target string.
CharToHex_Loop:
    PUSH di
    mov di, offset Buffer_On_Row
    add di, si
    mov al, [di]
    POP di
    mov ah, al

    and al, 000fh
    xlat
    add di, 1d
    mov [di], al
    ; =======================================
    ; [CheckPoint] Check al conversion.
    ; Test1: buffer="1"(1 byte, 48)
    ;       Expect: al=0 . [PASSED]
    ; =======================================

    mov al, ah
    
    push cx
    mov cl, 4d

    push cx
    mov cl, 4d
    shr al, cl      ; Convert higher 4 bits 
    pop cx

    pop cx
    
    and al, 000fh
    xlat
    sub di, 1d
    mov [di], al

    add di, 3d ; skip space or | 
    inc si
    cmp si, cx
    jb CharToHex_Loop
    ; =======================================
    ; [CheckPoint] Check final convension
    ;   [PASSED]
    ; =======================================

    POP DX
    POP CX
    POP BX
    POP AX
	POP si
    POP di
    ret
CharToHex_ ENDP 


C2H PROC NEAR
; =======================================
;    Convert 
; =======================================
    ret
C2H ENDP

GetRow_ PROC near
    push ax 
    push di

    mov di, Offset Byte_in_buf
    mov ax, [di]
    ; and ah, 0d
    add ax, 15

    push cx
    mov cl, 4d
    shr ax, cl       ; Div 16.
    pop cx

    mov di, offset rows
    mov [di], al
    ; ============================/test===========
    ; [CheckPoint] Check ax value
    ; Test1: Input 15: AX = 0   [PASSED]
    ; Test2: Input 255: AX = 16 [PASSED]
    ; =======================================

    pop di
    pop ax
    ret
GetRow_ ENDP   

ClearThisPage_ PROC NEAR
    PUSH ax
    push cx
    push di

    mov ax, 0B800h
    mov es, ax
    mov di, 0000h
    ; es:di points at 0xB8000000
    mov ax, 0020h
    mov cx, 80d*16d
    cld
    rep stosw
    ; =======================================
    ; [CheckPoint] Check if screen cleared
    ;   [PASSED]
    ; =======================================
    ; For test purpose.
    ; mov ax, 0100h
    ; int 21h
    pop ax
    pop cx
    pop di
    ret
ClearThisPage_ ENDP

GetBytesOnRow_ PROC near
    ; =============================================
    ; Get the length of current row.
    ;   Input: SI (Current Row) & DI (Total Row)
    ;   return 16 if not the last row. (Di - si = 1)
    ;   Output Bytes_on_row
    ; =============================================
    push cx
    push di
    push si

    sub di, si
    cmp di, 1d
    je GetBytesOnRow_Ending

    mov cl, 16d
    jmp GetBytesOnRow_Save

GetBytesOnRow_Ending:
    mov di, offset Byte_in_buf
    mov cx, [di]
    ; mov cl, [di]
    ; and ch, 0d
    push cx
    mov cl, 4d
    shl si, cl              ; SI *= 16
    pop cx

    sub cx, si
    ; =======================================
    ; [CheckPoint] Check sub result cx
    ; Test1: byte_in_buf = 254d.
    ;   Expect: ch = 00d cl = 15d (0E) [PASSED]
    ; =======================================

GetBytesOnRow_Save:
    mov di, offset Byte_on_row
    mov [di], cl

    pop si
    pop di
    pop cx
    ret
GetBytesOnRow_ ENDP

GetPointer_ PROC near
    ; =======================================
    ;  Convert 32 bit location into the pattern
    ;       Input: Offsets
    ;       Output: Pattern
    ; =======================================
    push ax
    push bx
    push cx
    push di
    push si
    
    mov di, offset template_S[7]
    mov si, offset location_off
    mov cl, 00d
GetPointer_Off:
    mov ax, [si]
    ; =======================================
    ; [CheckPoint] Check AX
    ; Test1: AX = 0CABF. [PASSED]
    ;   Failed1: Memory small end.
    ; =======================================
    shr ax, cl
    and ax, 000fh ; Clear high bits but leaving least 4 bits.

    ; Write in low 4 bits
    ; mov byte ptr al, [si]
    mov bx, offset conv_table
    xlat
    ; =======================================
    ; [CheckPoint] CHECK IF XLAT works. [WORKS]
    ; =======================================
    mov [di], al
    dec di 

    add cl, 4
    cmp cl, 16d
    jne GetPointer_Off
    ; =======================================
    ; [CheckPoint] Check pattern.
    ; Test1: 0fABCh, 0CABFh. Expect: as input.
    ; =======================================
    ; mov di, offset pattern[3]


    mov di, offset template_S[3]   ; Place di at the end of pattern string.
    mov si, offset location_seg
    mov cl, 00d
GetPointer_Seg:
    mov ax, [si]

    shr ax, cl
    and ax, 000fh ; Clear high bits but leaving least 4 bits.

    mov bx, offset conv_table
    xlat
    mov [di], al
    dec di 

    add cl, 4
    cmp cl, 16d
    jne GetPointer_Seg

    ; mov di, offset pattern
    ; =======================================
    ; [CheckPoint] Check Pointer pattern
    ; Test1: 0fABCh, 0CABFh. Expect: as input.
    ;       [PASSED]
    ; =======================================
    pop si
    pop di
    pop cx
    pop bx
    pop ax
    ret
GetPointer_ ENDP

GetOffsetOfRow_ PROC near
; =======================================
;  Update offset for each row.
;       Input: SI (Current row number) 
;           offset (of current page)
;       Output: location_off / seg
;  [location_seg:off] = offset + si * 16d
; =======================================
    PUSH DI
    PUSH AX
    PUSH SI
    PUSH DX

    mov di, offset offsets[0]
    mov ax, [di]               ; load current page offsets.
    mov di, si
    mov dx, 00d

    push cx
    mov cl, 4d
    shl si, cl ; I * 16
    pop cx
    add ax, si
    adc dx, 00h
    mov di, offset location_off  ; Write in low 16 bit location.
    mov [di], ax

    mov di, offset offsets[2]
    mov ax, [di]
    add ax, dx
    mov di, offset location_seg
    mov [di], ax                 ; Write in high 16 bit location.
    ; =======================================
    ; [CheckPoint] Check location offset changes
    ; =======================================
	POP DX
    POP SI
    POP AX
    POP DI
    ret
GetOffsetOfRow_ ENDP

code ends
end main