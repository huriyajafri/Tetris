[org 0x100]
jmp start

; ----------------------<( VARIABLES )>----------------------
scoreStr: db 'SCORE ', 0
timeStr: db 'TIME ', 0
nxt:db 'NEXT SHAPE ', 0
Msg1: db 'Press "Enter" To Continue',0      ;Press 'Enter' To Continue
Msg2: db 'Score: ',0      ;Press 'Enter' To Continue
scorenum: dw 0
endmsg:db '---------GAME OVER--------', 0
temp: dw 0
nextshape: dw 1
x: dw 0
y: dw 24
flag: dw 1
oldisr: dd 0
second1: dw 0
second2: dw 0
minutes:dw 0
tickcount: dw 0

; ----------------------<( SCROLL )>------------------------------
Scroll:
	push bp
	mov bp,sp
	push ax
	push cx
	push si
	push di
	push es
	push ds
	
	mov ax,0xb800
	mov es,ax 
	mov ds,ax 
	
	mov di,[bp+4] ;bp+4 holds starting point of the full row that needs to be removed
	add di,110 ;our frame is of 55 columns, pointing to the end of the full row
	mov si,di
	sub si,160 ;setting source one row above the fully colored row
	
	scrollrow:
		push di
		push si
		mov cx,56 ;width
		
		std 
		rep movsw
		
		pop si
		pop di
		
		;moving si and di to point one row above the current positions:
		sub di,160
		sub si,160
		cmp si,0 ;0 is the starting point of out main frame
		jg scrollrow
		
	;printing top row of the main frame:
	mov ax,7820h 
	mov di,0
	mov cx,56
	cld
	rep stosw
	pop ds
	pop es
	pop di
	pop si
	pop cx
	pop ax
	pop bp
	ret 2

; ----------------------<( SCAN SCREEN )>----------------------
Scanscr:
	push bp
	mov bp,sp
	push ax
	push cx
	push di
	push es
	
	push 0xB800
	pop es
	mov ax,1e20h 	;attributes of stopped shapes
	mov di,0 		;starting point of main frame
	mov cx,23 		;number of rows of main frame
	scanRow:
		push di
		push cx
		mov cx,57	;width of main frame
		cld
		repe scasw
		cmp cx,0      
		pop cx
		pop di
		jne nextIteration  ;to chk next row without doing anything
		call changescore   ;change score scroll n check next row
		inc word[x]
		push di
		call Scroll
		nextIteration:
		add di,160
		loop scanRow
	pop es
	pop di
	pop cx
	pop ax
	pop bp
	ret

; ----------------------<( KEYBOARD INTERRUPT SERVICE ROUTINE )>----------------------
kbisr: 
push ax
in al, 0x60 ; read a char from keyboard port
cmp al, 0x4b ; has the left shift pressed
jne nextcmp ; no, try next comparison
call moveleft
jmp exitt ; leave interrupt routine
nextcmp: cmp al, 0x4d ; has the right shift pressed
jne exitt ; no, try next comparison
call moveright

exitt: mov al, 0x20
out 0x20, al ; send EOI to PIC
pop ax
iret ; return from interrupt


; ----------------------<( CLEAR SCREEN )>----------------------
clrscr:
push es
push ax
push di
mov ax, 0xb800
mov es, ax 
mov di, 0 
nextloc: mov word [es:di], 0x0720 
add di, 2 
cmp di, 4000 
jne nextloc 
pop di
pop ax
pop es
ret


; ----------------------<( END SCREEN )>----------------------
endscreen:
call clrscr
call gameover

;;;;;;;;;;;;;;;;;;;;;;;
 mov cx, 5
loopyy:         mov al, 0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
call delay1

mov al, ah
out 61h, al

call delay1

;load the counter 2 value for a3
mov ax, 152fh
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
call delay1


mov al, ah
out 61h, al

call delay1
	
;load the counter 2 value for a4
mov ax, 0A97h
out 42h, al
mov al, ah
out 42h, al
	
;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
call delay1

mov al, ah
out 61h, al

call delay1
 
 loop loopyy

;;;;;;;;;;;;;;;;;;;;;;;;

mov ax, 36
push ax ; push x position
mov ax, 13
push ax ; push y position
mov ax, 0x0F
;or  ax, 0x80 ; set the blinking attribute bit
push ax ; push attribute
mov ax, Msg2
push ax ; push offset of string
call printstr ; print the string


mov ax, 43
push ax ; push y position
mov ax, 13
push ax ; push s position
mov ax,0x0f
push ax 
mov ax,[scorenum]
push ax 
call printnum

ret

; ----------------------<( GAME OVER )>----------------------
gameover:
;;;;;;g
mov ax, 0 ;row bp+8
push ax 
mov ax, 1;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 8 ;row bp+8
push ax 
mov ax, 1;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 0 ;row bp+8
push ax 
mov ax, 1 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 7 ;row bp+8
push ax 
mov ax, 6 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapebb

mov ax, 8 ;row bp+8
push ax 
mov ax, 6 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapebb
;;;;;;;;;;;;;;

;;;;;;;;;;;;a
mov ax, 0 ;row bp+8
push ax 
mov ax, 11;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 4 ;row bp+8
push ax 
mov ax, 11;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 2 ;row bp+8
push ax 
mov ax, 11 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 2 ;row bp+8
push ax 
mov ax, 16 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 0 ;row bp+8
push ax 
mov ax, 16 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapebb

;;;;;;;;;;;;;m
mov ax, 0 ;row bp+8
push ax 
mov ax, 21 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 2 ;row bp+8
push ax 
mov ax, 21 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 0 ;row bp+8
push ax 
mov ax, 27 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 2 ;row bp+8
push ax 
mov ax, 27 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 1 ;row bp+8
push ax 
mov ax, 25 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapebb
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;E
mov ax, 0 ;row bp+8
push ax 
mov ax, 32;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 4 ;row bp+8
push ax 
mov ax, 32;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 8 ;row bp+8
push ax 
mov ax, 32;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 0 ;row bp+8
push ax 
mov ax, 32 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22
;;;;;;;

;;;;;;;;;o
mov ax, 0;row bp+8
push ax 
mov ax, 42;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape2a

mov ax, 2;row bp+8
push ax 
mov ax, 42;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape2a

mov ax, 0;row bp+8
push ax 
mov ax, 44;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape2a

mov ax, 2;row bp+8
push ax 
mov ax, 44;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape2a

mov ax, 4;row bp+8
push ax 
mov ax, 43;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeb

;;;;;;

;;;;;;;;;;;v
mov ax, 3 ;row bp+8
push ax 
mov ax, 49;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapev1

mov ax, 3 ;row bp+8
push ax 
mov ax, 55;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapev2

mov ax, 5 ;row bp+8
push ax 
mov ax, 52;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapev

;;;;;;;;;;E
mov ax, 0 ;row bp+8
push ax 
mov ax, 60;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 4 ;row bp+8
push ax 
mov ax, 60;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 8 ;row bp+8
push ax 
mov ax, 60;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 0 ;row bp+8
push ax 
mov ax, 60 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22
;;;;;;;

;;;;;;;;;;;;;;;;;;;;R
mov ax, 0 ;row bp+8
push ax 
mov ax, 70;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 4 ;row bp+8
push ax 
mov ax, 70;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 2 ;row bp+8
push ax 
mov ax, 70 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 0 ;row bp+8
push ax 
mov ax, 75 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 2;row bp+8
push ax 
mov ax, 75 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22
;;;;;;;;;;;
call drawblocks
ret

; ----------------------<( BACKGROUND )>----------------------
background:
push es
push ax
push di
mov ax, 0xb800
mov es, ax
mov di, 0 
nextloc2: 
mov word [es:di], 0x7820 
add di, 2 
cmp di, 4000 
jne nextloc2

pop di
pop ax
pop es
ret

; ----------------------<( PARTITION )>----------------------
partition:
push es
push ax
push di
push si

mov ax, 0xb800
mov es, ax
mov di,112
mov si,160
mov bx,di

nextpos:
mov word [es:di], 0x3e20
add di, 2 
cmp di,si
jne nextpos

incrow:
add si,160
add bx,160
mov di,bx
cmp si,4160
jne nextpos



mov si,3680
mov bx,si
add bx,112
mov di,si
p:
mov word [es:di], 0x6620
add di, 2 
cmp di,bx
jne p
add si,160
add bx,160
mov di,si
cmp si,4000
jne p


mov ax, 3 ;row bp+8
push ax 
mov ax, 62 ;col bp+6
push ax 
mov ax,0x03 ;bp+4
push ax ; push attribute
call box


mov ax, 8 ;row bp+8
push ax 
mov ax, 62 ;col bp+6
push ax 
mov ax,0x03 ;bp+4
push ax ; push attribute
call box


pop si
pop di
pop ax
pop es
ret


; ----------------------<( DELAY )>----------------------
delay:
push bx
push cx
mov cx,0xffff
delaying 
add bx,1
loop delaying
mov cx,0xffff
d1: 
add bx,1
loop d1
mov cx,0xffff
d2: 
add bx,1
loop d2
mov cx,0xffff
d3: 
add bx,1
loop d3
pop cx
pop bx
ret


; ----------------------<( PRINT STRING )>----------------------
printstr: 
push bp
mov bp, sp
push es
push ax
push cx
push si
;push di
push ds
pop es ; load ds in es
mov di, [bp+4] ; point di to string
mov cx, 0xffff ; load maximum number in cx
xor al, al ; load a zero in al
repne scasb ; find zero in the string
mov ax, 0xffff ; load maximum number in ax
sub ax, cx ; find change in cx
dec ax ; exclude null from length
jz exit ; no printing if string is empty
mov cx, ax ; load string length in cx
mov ax, 0xb800
mov es, ax ; point es to video base
mov al, 80 ; load al with columns per row
mul byte [bp+8] ; multiply with y position
add ax, [bp+10] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di to required location
mov si, [bp+4] ; point si to string
mov ah, [bp+6] ; load attribute in ah
cld ; auto increment mode
nextchar: lodsb ; load next char in al
stosw ; print char/attribute pair
loop nextchar ; repeat for the whole string
exit: 
;pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 8

shape2a:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block

add ax,2
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block

pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shapev:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x56
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shapebb:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6



shapev1:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x3E
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shapev2:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x42
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6


; ----------------------<( BOX )>----------------------
box:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
push bx

mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute

mov si,3
l:
push ax ;row
push bx ;col
push cx ; push attribute
call block
add bx,4
dec si
cmp si,0
jne l


pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6


; ----------------------<( PRINT NUMBER )>----------------------
printnum: 
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di


mov al, 80 ; load al with columns per row
mul byte [bp+8] ; multiply with y position
add ax, [bp+10] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di to required location

mov ax, 0xb800
mov es, ax 
mov ax,[bp+4] 
mov bx, 10 
mov cx, 0 
nextdigit: mov dx, 0 
div bx ; divide by 10
add dl, 0x30 
push dx 
inc cx 
cmp ax, 0 
jnz nextdigit

nextpos1: 
pop dx 
mov dh,[bp+6]
mov [es:di], dx 
add di, 2 
loop nextpos1
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 8

; ----------------------<( PRINT TIME )>----------------------
printtime: 
    push bp 
    mov  bp, sp
    push es 
    push ax 
    push bx 
    push cx 
    push dx 
    push di 
    mov ax, [bp+4] 
    mov bx, 10       
    mov cx, 0       
    nextdigi: 
        mov dx, 0    
        div bx      
        add dl, 0x30 
        push dx     
        inc cx       
        cmp ax, 0   
        jnz nextdigi 
    mov ax, 0xb800 
    mov es, ax 
    mov di, [bp+6]
    nextp: 
        pop dx    
        mov dh, 0x0a 
        mov [es:di], dx 
        add di, 2 
        loop nextp   
    pop di 
    pop dx 
    pop cx 
    pop bx 
    pop ax 
    pop es
    pop bp 
    ret 4 

; ----------------------<( timer interrupt service routine )>----------------------
timer:
	push ax
	cmp word[cs:minutes],5
	je kl
	inc word [cs:tickcount]; increment tick count
	cmp word[cs:tickcount],18
	je update
	cmp word[cs:tickcount],18
	jne k2
update:
    mov word[cs:tickcount],0
	inc word[cs:second2]
    cmp word[cs:second2],10
	jl k2
	mov word[cs:second2],0
	inc word[cs:second1]
	cmp word[cs:second1],6	
	jne k2
	inc word [cs:minutes] 
	mov word [cs:second1],0
k2:
    mov ax,1572
	push ax
	push word [cs:minutes]
	call printtime
	
	mov ax,0xb800
		mov es,ax
		mov ah,0x0a
		mov al,':'
		mov word[es:787*2],ax
		
    mov ax,1576
	push ax
	push word [cs:second1]
	call printtime
	
	mov ax,1578
	push ax
	push word [cs:second2]
	call printtime
	
kl:
mov al, 0x20
out 0x20, al ; end of interrupt
pop ax
iret ; return from interrupt


; ----------------------<( SCORE ADD )>----------------------
changescore:
push ax
add word[scorenum],10
mov ax,67
push ax ; push y position
mov ax, 4
push ax ; push x position
mov ax,0x0e
push ax 
mov ax,[scorenum]
push ax 
call printnum
pop ax
ret


; ----------------------<( MAIN SCREEN )>----------------------
mainscr:
call clrscr
call background
call delay
call partition

mov ax, 65
push ax ; push y position
mov ax, 1
push ax ; push x position
mov ax,0x30
push ax ; push attribute
mov ax, scoreStr
push ax ; push address of message
call printstr



mov ax,67
push ax ; push y position
mov ax, 4
push ax ; push x position
mov ax,0x0e
push ax 
mov ax,[scorenum]
push ax 
call printnum


mov ax, 66
push ax ; push y position
mov ax, 6
push ax ; push x position
mov ax,0x30
push ax ; push attribute
mov ax, timeStr
push ax ; push address of message
call printstr


mov ax, 63
push ax ; push y position
mov ax, 13
push ax ; push x position
mov ax,0x30
push ax ; push attribute
mov ax, nxt
push ax ; push address of message
call printstr
ret



; ----------------------<( BLOCK )>----------------------
block:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
push bx

mov ax, 0xb800
mov es, ax ; point es to video base
mov al, 80 ; load al with columns per row
mul byte [bp+8] ;row
add ax, [bp+6] ;col
shl ax, 1 ; turn into byte offset
mov bx,ax
mov di,ax ; point di to required location
mov al,' '
mov ah, [bp+4] ; load attribute in ah
mov cx,4
mov si,2
l1:
mov word [es:di], ax
add di, 2 
loop l1
mov cx,4
add bx,160
mov di,bx
dec si
cmp si,0
jne l1

 
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shape11:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


add bx,4
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

add bx,4
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shape9:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


add bx,4
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6



shape22:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shape7:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shapeb:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x07
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6




shapeq:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
push cx ; push attribute
call block


add bx,4
push ax ;row
push bx ;col
mov cx,0x3E
push cx ; push attribute
call block

add bx,4
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6



shapew:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

add ax,2
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6




shapee:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx,0x3E
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

add bx,4
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shaper:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
mov cx, 0x3E
push cx ; push attribute
call block


add bx,4
push ax ;row
push bx ;col
mov cx,0x1e
push cx ; push attribute
call block

add ax,2
push ax ;row
push bx ;col
mov cx,0x45
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shape1:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
push dx

mov ax,[bp+8] ;row
mov bx,[bp+6] ;col

mov dx,[bp+4] ;attribute
mov cx,3
draw:
push ax ;row
push bx ;col
push dx ; push attribute
call block
add bx,4
loop draw


pop dx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6



shape2:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
push dx

mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov dx,[bp+4] ;attribute
mov cx,3



d:
push ax ;row
push bx ;col
push dx ; push attribute
call block
add ax,2
loop d

pop dx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6




shape3:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
push cx ; push attribute
call block


add ax,2
push ax ;row
push bx ;col
push cx ; push attribute
call block

add bx,4
push ax ;row
push bx ;col
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

shape4:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di


mov ax,[bp+8] ;row
mov bx,[bp+6] ;col
mov cx,[bp+4] ;attribute


push ax ;row
push bx ;col
push cx ; push attribute
call block


add bx,4
push ax ;row
push bx ;col
push cx ; push attribute
call block

add ax,2
push ax ;row
push bx ;col
push cx ; push attribute
call block


pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 6

; ----------------------<( START )>----------------------
startscr:
call clrscr
call startscreen

mov ah, 0x10 ; service 10 – vga attributes
mov al, 03 ; subservice 3 – toggle blinking
mov bl, 01 ; enable blinking bit
int 0x10 ; call BIOS video service


mov ax, 28
push ax ; push x position
mov ax, 13
push ax ; push y position
mov ax, 0x0F
or  ax, 0x80 ; set the blinking attribute bit
push ax ; push attribute
mov ax, Msg1
push ax ; push offset of string
call printstr ; print the string



ret

; ----------------------<( START SCREEN )>----------------------
startscreen:
;push ax

;;;;;;T
mov ax, 0 ;row bp+8
push ax 
mov ax, 0;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape11

mov ax, 2 ;row bp+8
push ax 
mov ax, 4 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22
;;;;;;;;;;;;;;


;;;;;;;;;;E
mov ax, 0 ;row bp+8
push ax 
mov ax, 15;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 4 ;row bp+8
push ax 
mov ax, 15;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 8 ;row bp+8
push ax 
mov ax, 15;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 0 ;row bp+8
push ax 
mov ax, 15 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22
;;;;;;;


;;;;;;;;;;;;T
mov ax, 0 ;row bp+8
push ax 
mov ax, 25;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape11

mov ax, 2 ;row bp+8
push ax 
mov ax, 29 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

;;;;;;;;;;;;;;;;;;;;R
mov ax, 0 ;row bp+8
push ax 
mov ax, 40;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 4 ;row bp+8
push ax 
mov ax, 40;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 2 ;row bp+8
push ax 
mov ax, 40 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22

mov ax, 0 ;row bp+8
push ax 
mov ax, 48 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape7

mov ax, 4 ;row bp+8
push ax 
mov ax, 48 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape7

mov ax, 4 ;row bp+8
push ax 
mov ax, 48 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeb
;;;;;;;;;;;


;;;;;;;;;;;;;;I
mov ax, 0 ;row bp+8
push ax 
mov ax, 55;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 8 ;row bp+8
push ax 
mov ax, 55;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape9

mov ax, 0 ;row bp+8
push ax 
mov ax, 57 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape22
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;S
mov ax, 0 ;row bp+8
push ax 
mov ax, 67;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape11

mov ax, 4 ;row bp+8
push ax 
mov ax, 66;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape11

mov ax, 8 ;row bp+8
push ax 
mov ax, 66;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape11

mov ax, 0 ;row bp+8
push ax 
mov ax, 66 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape7

mov ax, 4 ;row bp+8
push ax 
mov ax, 75 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shape7
call drawblocks

;pop ax
ret

; ----------------------<( DRAW BLOCKS )>----------------------
;+++++++++++++++++++++++++++++++
drawblocks:
;;;;;;;;;;;;;;;;;blocks
mov ax, 23 ;row bp+8
push ax 
mov ax, 1 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeq

mov ax, 17;row bp+8
push ax 
mov ax, 1 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapew

mov ax, 19;row bp+8
push ax 
mov ax, 9 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapee

mov ax, 21;row bp+8
push ax 
mov ax, 17;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shaper

mov ax, 23 ;row bp+8
push ax 
mov ax, 25 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeq

mov ax, 19;row bp+8
push ax 
mov ax, 29 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapee

mov ax, 21;row bp+8
push ax 
mov ax, 37;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shaper

mov ax, 23 ;row bp+8
push ax 
mov ax, 45 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeq

mov ax, 17;row bp+8
push ax 
mov ax, 45 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapew

mov ax, 19;row bp+8
push ax 
mov ax, 53 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapee

mov ax, 21 ;row bp+8
push ax 
mov ax, 61 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeq

mov ax, 21;row bp+8
push ax 
mov ax, 69 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapew

mov ax, 19;row bp+8
push ax 
mov ax, 73 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapew

mov ax, 15;row bp+8
push ax 
mov ax, 69;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapee

mov ax, 19 ;row bp+8
push ax 
mov ax, 57 ;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeq

mov ax, 17;row bp+8
push ax 
mov ax, 21;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapee

mov ax, 13 ;row bp+8
push ax 
mov ax, 9;col bp+6
push ax 
mov ax,0x57 ;bp+4
push ax ; push attribute
call shapeq

ret
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ----------------------<( SHAPES PRINTING )>----------------------
printshapes:

mov word[flag],1
call checkspace
cmp word[flag],0
jne donotstop
jmp bck
donotstop:
mov word[x],0
mov word[y],24
mov ax,[nextshape]
mov [temp],ax
add word[nextshape],1

cmp word[nextshape],5
jne noupdate

mov word[nextshape],1
noupdate:

cmp word[temp],1
jne check2
mov ax, [x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape1
call clearspace
mov ax, 15;row bp+8
push ax 
mov ax, 66 ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape2

jmp bck

check2:
cmp word[temp],2
jne check3
mov ax, [x] ;row bp+8
push ax 
mov ax, [y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape2
call clearspace
mov ax, 15;row bp+8
push ax 
mov ax, 64 ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape3

jmp bck
check3:
cmp word[temp],3
jne check4

mov ax, [x];row bp+8
push ax 
mov ax, [y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape3
call clearspace
mov ax, 15;row bp+8
push ax 
mov ax, 64 ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape4

jmp bck

check4:

mov ax,[x];row bp+8
push ax 
mov ax, [y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape4
call clearspace
mov ax, 15;row bp+8
push ax 
mov ax, 62 ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape1

bck:
ret



; ----------------------<( DOWN )>----------------------

down1:
push ax
push cx
;push di

add di,320
mov cx,13
push 0xb800
pop es
mov ax,0x7820
cld
repe scasw
cmp cx,0
jne nomov

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape1



add word[x],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape1

jmp exitdown1

nomov:
call Scanscr


mov al,0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al
call printshapes
exitdown1:
;pop di
pop cx
pop ax
ret



down2:
push ax
push cx
;push di

add di,960
mov cx,5

mov ax,0x7820
cld
repe scasw
cmp cx,0
jne nomo

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape2

add word[x],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape2
jmp exitdown2
nomo:
call Scanscr

mov al,0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al
call printshapes
exitdown2:
;pop di
pop cx
pop ax
ret

down3: 
push ax
push cx
;push di

add di,640
mov cx,9

mov ax,0x7820
cld
repe scasw
cmp cx,0
jne nomov3

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape3

add word[x],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape3
jmp exitdown3
nomov3:
call Scanscr

mov al,0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al
call printshapes
exitdown3:
;pop di
pop cx
pop ax
ret


down4: 
push ax
push cx

add di,648
mov cx,5

mov ax,0x7820
cld
repe scasw
cmp cx,0
jne nomov4

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape4

add word[x],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape4
jmp exitdown4
nomov4:
call Scanscr

mov al,0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
mov al, ah
out 61h, al
call printshapes
exitdown4:
pop cx
pop ax
ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ----------------------<( LEFT )>----------------------
left1:
push ax
push cx
;push di

sub di,2
mov cx,13
push 0xb800
pop es
mov dx,0x7820
cmp dx,[es:di]
jne nomovement

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape1

sub word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape1

nomovement:
;pop di
pop cx
pop ax
ret



left2:
push ax
push cx
;push di

sub di,2
mov cx,7

mov ax,0x7820
chkloop:
cmp ax,[es:di]
jne nomove2
add di,160
loop chkloop

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape2

sub word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape2

nomove2:
;pop di
pop cx
pop ax
ret

left3: 
push ax
push cx
;push di

sub di,2
mov cx,5

mov ax,0x7820
chkloop2:
cmp ax,[es:di]
jne back6
add di,160
loop chkloop2


mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape3

sub word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape3

back6:
;pop di
pop cx
pop ax
ret


left4: 
push ax
push cx
;push di


sub di,2
mov cx,5

mov ax,0x7820
chkloop3:
cmp ax,[es:di]
jne back7
add di,160
loop chkloop3

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape4

sub word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape4

back7:
;pop di
pop cx
pop ax
ret
;***************************************************

; ----------------------<( RIGHT )>----------------------
right1:
push ax
push cx
;push di

add di,24

push 0xb800
pop es
mov dx,0x7820
cmp dx,[es:di]
jne nomovement2

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape1

add word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape1

nomovement2:
;pop di
pop cx
pop ax
ret



right2:
push ax
push cx
;push di

add di,8
mov cx,6

mov ax,0x7820
chekloop:
cmp ax,[es:di]
jne nomovemen2
add di,160
loop chekloop

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape2

add word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape2

nomovemen2:
;pop di
pop cx
pop ax
ret

right3: 
push ax
push cx
;push di

add di,16
mov cx,4

mov ax,0x7820
chekloop2:
cmp ax,[es:di]
jne bck6
add di,160
loop chekloop2


mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape3

add word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape3

bck6:
;pop di
pop cx
pop ax
ret


right4: 
push ax
push cx
;push di


add di,16
mov cx,4

mov ax,0x7820
chekloop3:
cmp ax,[es:di]
jne bck7
add di,160
loop chekloop3

mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x78 ;bp+4  ;to clear original block space giving the background colour
push ax ; push attribute
call shape4

add word[y],1
mov ax,[x] ;row bp+8
push ax 
mov ax,[y] ;col bp+6
push ax 
mov ax,0x1e ;bp+4
push ax ; push attribute
call shape4

bck7:
;pop di
pop cx
pop ax
ret


; ----------------------<( MOVEDOWN )>----------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
movedown:
push ax
mov dx,0
mov ax, 80 ; load al with columns per row
mul word[x] ; multiply with y position
add ax, [y] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di
mov ax,0xb800
mov es,ax
cmp word[temp],1
jne c2
call down1
jmp back
c2:
cmp word[temp],2
jne c3
call down2
jmp back
c3:
cmp word[temp],3
jne c4
call down3
jmp back
c4:
cmp word[temp],4
jne back
call down4

back:
pop ax
ret


; ----------------------<( MOVE LEFT )>----------------------
moveleft:
push ax
mov dx,0
mov ax, 80 ; load al with columns per row
mul word[x] ; multiply with y position
add ax, [y] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di
mov ax,0xb800
mov es,ax
cmp word[temp],1
jne cl2
call left1
jmp back5
cl2:
cmp word[temp],2
jne cl3
call left2
jmp back5
cl3:
cmp word[temp],3
jne cl4
call left3
jmp back5
cl4:
cmp word[temp],4
jne back5
call left4

back5:
pop ax
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ----------------------<( MOVERIGHT )>----------------------
moveright:
push ax
mov dx,0
mov ax, 80 ; load al with columns per row
mul word[x] ; multiply with y position
add ax, [y] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di
mov ax,0xb800
mov es,ax
cmp word[temp],1
jne r2
call right1
jmp endd
r2:
cmp word[temp],2
jne r3
call right2
jmp endd
r3:
cmp word[temp],3
jne r4
call right3
jmp endd
r4:
cmp word[temp],4
jne endd
call right4

endd:
pop ax
ret
;--------------------------------------------------------------;

; ----------------------<( CLEAR SPACE )>----------------------
clearspace:
push ax
push cx
push bx
push di
push si
mov di,0
add di,2*62
add di,14*160
mov si,di
mov ax,0xb800
mov es,ax
mov cx,8
clearrow: 
mov bx,12   
clearcolumn:
mov word[es:di],0x3e20
add di,2
dec bx
cmp bx,0
jne clearcolumn
add si,160
mov di,si
loop clearrow
pop si
pop di
pop bx
pop cx
pop ax
ret
;--------------------------------------------------------------;

; ----------------------<( CHECK SPACE )>----------------------
checkspace:
push ax
push cx
push bx
push si
push di
mov word[flag],1
mov di,48
mov si,di
mov ax,0xb800
mov es,ax
mov cx,6
checkrow: 
mov bx,12   
checkcolumn:
cmp word[es:di],0x7820
jne changeflag
add di,2
dec bx
cmp bx,0
jne checkcolumn
add si,160
mov di,si
loop checkrow
jmp outoffunc
changeflag:
mov word[flag],0
outoffunc:
pop di
pop si
pop bx
pop cx
pop ax
ret


; ----------------------<( GAME )>----------------------
game:

xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [es:9*4]
mov [oldisr], ax ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldisr+2], ax ; save segment of old routine

push word[es:8*4]
push word[es:8*4+2]
cli ; disable interrupts
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2

mov word [es:8*4], timer
mov [es:8*4+2], cs 
sti ; enable interrupts
continueplaying:

call printshapes
loopdown:
call delay
call delay
call movedown
cmp word[flag],0
je exittt
cmp word[cs:minutes],5
jne loopdown
exittt:
xor ax,ax
mov es,ax
cli
pop word[es:8*4+2]
pop word[es:8*4]
mov ax,[oldisr]
mov [es:9*4],ax
mov ax,[oldisr+2]
mov [es:9*4+2],ax
sti 
ret
;----------------------------------------------------------------------;
;---------------------------     DELAY FOR SOUND     ---------------------------;
delay1: push cx
mov cx,0xffff
ql1: loop ql1
pop cx
ret

; ----------------------<( MAIN )>----------------------
start:
call startscr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SOUND GENERATE
mov cx, 5
loop1:         mov al, 0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
call delay1
call delay1
call delay1
mov al, ah
out 61h, al

call delay1

;load the counter 2 value for a3
mov ax, 152fh
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
call delay1
call delay1
call delay1

mov al, ah
out 61h, al

call delay1
	
;load the counter 2 value for a4
mov ax, 0A97h
out 42h, al
mov al, ah
out 42h, al
	
;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay1
call delay1
call delay1
call delay1
mov al, ah
out 61h, al

call delay1
 
 loop loop1


mov ah,0x1
int 21h

call mainscr
call game

call endscreen

mov ah,0x1
int 21h

mov ax,0x4c00
int 21h