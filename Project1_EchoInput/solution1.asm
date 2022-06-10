data segment
	s    db 100 dup(0)
	t    db 100 dup(0)      ; Init data segment with 0.
data ends

code segment
	            assume cs: code, ds: data
	main:       
	            mov    ax, data
	            mov    ds, ax         ; DS do not receive segment location directly.

	            mov    ah, 01h        ; Int 21. Input from stdin.
	            mov    si, 00h        ; Iterative variable.
	input:      					
	            int    21h
	            mov    [si],al
	            inc    si
	            cmp    al, 0Dh		  ; Check if ending with enter.
	            jne    input
	            mov    byte ptr [si-1],00h  
	; CheckPoint: Check content of s. [PASSED]
	            mov    si, 00h            	; Reset si and di for traverse.
	            mov    di, offset t
	; CheckPoint: Check content of di. [PASSED: FIXED 64h]
	            mov    bl, 61h
	            mov    bh, 7Ah
	traverse:   
	            cmp    byte ptr [si], 00h   
	            je     output             	; Iteration ends with 00h. 
	            cmp    byte ptr [si], 20h
	            je     space              	; If space detected, jump directly to next character.
	            cmp    [si], bl
	            jb     uppercase
	            cmp    [si], bh
	            ja     uppercase
	lowercase:  
	            mov    cl, [si]
	            sub    cl, 20h            	; Convert upper case to lower.
	            mov    [di], cl
	            inc    di
	            inc    si
	            jmp    traverse
	space:      
	            inc    si
	            jmp    traverse
	uppercase:  
	            mov    cl, [si]
	            mov    [di], cl
	            inc    di
	            inc    si
	            jmp    traverse
	output:     
	            mov    si, offset t         ; Reset si for iteration.
	            mov    ah, 02h              ; Int 21h-02: Output to stdout.
	output_loop:
	            mov    dl, [si]
	            inc    si
	            int    21h
	            cmp    si, di
	            jb     output_loop

	            mov    dl, 0Dh
	            int    21h
	            mov    dl, 0Ah
	            int    21h                  ; Output {BackSpace} for termination.

	            mov    ah, 4ch            	; Exit.
	            int    21h
code ends
end main