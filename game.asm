IDEAL
MODEL small
 
 

STACK 0f500h


 
BMP_WIDTH = 320
BMP_HEIGHT = 200

SMALL_BMP_HEIGHT = 40
SMALL_BMP_WIDTH = 40

MOUSE_COLORred equ 127 ; red
MOUSE_COLORgreen equ 255 ; green
MOUSE_COLORblue equ 127 ; blue



 
FILE_NAME_IN equ 'Open.bmp'

DATASEG

    OneBmpLine 	db BMP_WIDTH dup (0)  ; One Color line read buffer
   
    ScrLine 	db BMP_WIDTH dup (0)  ; One Color line read buffer

	;BMP File data
	FileName 	db FILE_NAME_IN ,0
	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)
	leftclick db 0
	
	game db 'Game.bmp',0
	button_name db 'button.bmp', 0
	
	matrix dw ?
	spaceship_place dw ?
	rect_width dw 15
	rect_hight dw 25
	rect_col dw 10
	rect_row db 0
	row_num db 0
	big_circle_color db 8
	small_circle_color db 11
	game_end db 0
	temp_row_num db ?
	color_write db 'Color:$'
	score_write db 'Score:$'
	;08CA:0736 - 0771
	row_array db 10 dup(?)
			  db 10 dup(?)
			  db 10 dup(?)
			  db 10 dup(?)
			  db 10 dup(?)
			  db 10 dup(?)
	
	BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILE_NAME_IN, 0dh, 0ah,'$'
	random_number db ?
	row_mult db 10
	y_mult db 27
	RndCurrentPos dw 0
	ErrorFile db 0
	div_check dw 5
	
	score db 0
	score_length dw 0
	score_temp db ?
	score_to_write db '             '
	score_to_write_demo db '             '
	shot_div db 3
    BB db "BB..",'$' 
	shot_matrix db 3, 3, 3, 3, 3, 3
				db 3, 3, 3, 3, 3, 3
				db 3, 3, 3, 3, 3, 3
				db 3, 3, 3, 3, 3, 3
				db 3, 3, 3, 3, 3, 3
				db 3, 3, 3, 3, 3, 3

	; array for mouse int 33 ax=09 (not a must) 64 bytes
	
	 
	 
	Color db ?
	Xclick dw ?
	Yclick dw ?
	Xp dw ?
	Yp dw ?
	SquareSize dw ?
	 
	BmpLeft dw ?
	BmpTop dw ?
	BmpColSize dw ?
	BmpRowSize dw ?
	buttonXStart dw ?
	buttonXEnd dw ?
	buttonYStart dw ?
	buttonYEnd dw ?
	shotX dw ?
	shotY dw ?

CODESEG
 


 
start:
	mov ax, @data
	mov ds, ax
	
	call SetGraphic
 
	
	mov dx, offset FileName
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], 320
	mov [BmpRowSize] ,200
	call OpenShowBmp
	
	
	mov dx, offset button_name
	mov [BmpLeft],102
	mov [BmpTop],160
	mov [BmpColSize],100
	mov [BmpRowSize],32
	call OpenShowBmp
	
	
	call check_mouse_press
	
	call SetGraphic
		
	
	mov ax, 02h
	int 33h
	
	mov [spaceship_place], 150
	call draw_player
	
	call draw_screen
	
	call add_row
	
	mov bx,0
	mov dx, 0
down_loop:
	cmp bx, 60000
	jne move
	inc dx
	cmp dx, 3
	jne move
	mov dx, 0
	call add_row
	add [score], 5 ; remove!!!!!!!
move:
	call movment
	inc bx
	cmp [game_end], 1
	je game_stop
	
	jmp down_loop
	
game_stop:
	mov dx, offset game
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize],320
	mov [BmpRowSize],200
	call OpenShowBmp
exit:
	mov ah,0
    int 16h

    mov ah,7 ;silent input
    int 21h

    mov ax,2  ; back to mode text 
    int 10h

	
	mov ax, 4c00h
	int 21h
	

	
;==========================
;==========================
;===== Procedures  Area ===
;==========================
;==========================

proc OpenShowBmp near
	
	 
	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call ShowBMP
	
	 
	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp

 
 
	
; input dx filename to open
proc OpenBmpFile	near						 
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile
 
 
 



proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile




; Read 54 bytes the Header
proc ReadBmpHeader	near					
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader



proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette


; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
proc CopyBmpPalette		near					
										
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette


 
 
proc DrawHorizontalLine	near
	push si
	push cx
DrawLine:
	cmp si,0
	jz ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	 
	
	inc cx
	dec si
	jmp DrawLine
	
	
ExitDrawLine:
	pop cx
    pop si
	ret
endp DrawHorizontalLine



proc DrawVerticalLine	near
	push si
	push dx
 
DrawVertical:
	cmp si,0
	jz @@ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	
	 
	
	inc dx
	dec si
	jmp DrawVertical
	
	
@@ExitDrawLine:
	pop dx
    pop si
	ret
endp DrawVerticalLine



; cx = col dx= row al = color si = height di = width 
proc Rect
	push cx
	push di
NextVerticalLine:	
	
	cmp di,0
	jz @@EndRect
	
	cmp si,0
	jz @@EndRect
	call DrawVerticalLine
	inc cx
	dec di
	jmp NextVerticalLine
	
	
@@EndRect:
	pop di
	pop cx
	ret
endp Rect



proc DrawSquare
	push si
	push ax
	push cx
	push dx
	
	mov al,[Color]
	mov si,[SquareSize]  ; line Length
 	mov cx,[Xp]
	mov dx,[Yp]
	call DrawHorizontalLine

	 
	
	call DrawVerticalLine
	 
	
	add dx ,si
	dec dx
	call DrawHorizontalLine
	 
	
	
	sub  dx ,si
	inc dx
	add cx,si
	dec cx
	call DrawVerticalLine
	
	
	 pop dx
	 pop cx
	 pop ax
	 pop si
	 
	ret
endp DrawSquare




 
   
proc  SetGraphic
	mov ax,13h   ; 320 X 200 
				 ;Mode 13h is an IBM VGA BIOS mode. It is the specific standard 256-color mode 
	int 10h
	ret
endp 	SetGraphic

 

 
 
 


proc ShowBMP 
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
	mov cx,[BmpRowSize]
	
 
	mov ax,[BmpColSize] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	xor dx,dx
	mov si,4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok
	mov bp,4
	sub bp,dx

@@row_ok:	
	mov dx,[BmpLeft]
	
@@NextLine:
	push cx
	push dx
	
	mov di,cx  ; Current Row at the small bmp (each time -1)
	add di,[BmpTop] ; add the Y on entire screen
	
 
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	dec di
	mov cx,di
	shl cx,6
	shl di,8
	add di,cx
	add di,dx
	 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpColSize]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	
	pop dx
	pop cx
	 
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP 

proc CheckAndReadKey
	  mov ah ,1
	  int 16h
	  pushf
	  jz @@return 
	  mov ah ,0
	  int 16h

@@return:	
	  popf
	  ret
endp CheckAndReadKey



proc check_mouse_press
	push ax
	push bx
	push cx
	push dx
	
	push ax
	mov ax, 0h
	int 33h
	
	mov ax, 1h
	int 33h
	pop ax
	
	mov bx, 0
@@check_loop:
	mov ax, 3h
	int 33h
	shr cx, 1
	cmp bx, 01h
	jne @@check_loop
	je @@check_place
@@check_place:
	cmp dx, 160
	jb @@check_loop
	cmp dx, 195
	ja @@check_loop
	cmp cx, 102
	jb @@check_loop
	cmp cx, 202
	ja @@check_loop
exit_check:
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp check_mouse_press



; in dx how many cols 
; in cx how many rows
; in matrix - the bytes
; in di start byte in screen (0 64000 -1)

proc putMatrixInScreen
	push es
	push ax
	push si
	
	mov ax, 0A000h
	mov es, ax
	cld
	
	push dx
	mov ax,cx
	mul dx
	mov bp,ax
	pop dx
	
	
	mov si,[matrix]
	
NextRow:	
	push cx
	
	mov cx, dx
	rep movsb ; Copy line to the screen
	sub di,dx
	add di, 320
	
	
	pop cx
	loop NextRow
	
	
endProc:	
	
	pop si
	pop ax
	pop es
    ret
endp putMatrixInScreen

proc draw_player
	push ax
	push bx
	push cx
	push dx
	mov bx, 18 
@@loop:
	 mov dx,[spaceship_place]              ;column of circle center 
     mov di,180             ;row of circle center  
     mov al,[big_circle_color]               ;colour
	 mov cx, 0
	 push bx
	 call circleV2
	 pop bx
	 cmp bx, 1
	 je @@continue
	 dec bx
	 jmp @@loop
@@continue:
	mov bx, 10
@@loop1:
	mov dx,[spaceship_place]              ;column of circle center 
     mov di,180             ;row of circle center  
     mov al,[small_circle_color]           ;colour
	 mov cx, 0
	 push bx
	 call circleV2
	 pop bx
	 cmp bx, 1
	 je @@end
	 dec bx
	 jmp @@loop1
@@end:
	
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp draw_player



proc circleV2
                    mov       bp,0                ;x coordinate
                    mov       si,bx               ;y coordinate
c00:                call      _8pixels            ;set 8 pixels
                    sub       bx,bp               ;d=d-x
                    inc       bp                  ;x+1
					sub       bx,bp               ;d=d-(2x+1)
					jg        c01                 ;>> no step for y
                    add       bx,si               ;d=d+y
                    dec       si                  ;y-1
					add       bx,si               ;d=d+(2y-1)
c01:
					cmp       si,bp               ;check x>y
                    jae       c00                 ;>> need more pixels
                    ret
endp circleV2


proc _8pixels       
					call      _4pixels            ;4 pixels
                    xchg      bp,si               ;swap x and y
                    call      _2pixels            ;2 pixels
					neg       si
                    push      di
                    add       di,si
					call  FindScreenOffset
					cmp cx,1
					jz @@skipDown
					 cmp cx,3 
					jz @@skipRight
					mov       [es:di+bp],al
@@skipRight:
				 	sub       di,bp
					cmp cx,4 
					jz @@skipLeft
                    mov       [es:di],al
@@skipLeft:				
@@skipDown:				
					pop       di
                    ret 
endp _8pixels
				
				
proc _4pixels
                    xchg      bp,si               ;swap x and y
                    call      _2pixels            ;2 pixels
                    
					neg       si
                    push      di
                    add       di,si
					call  FindScreenOffset
					cmp cx,1
					jz @@skipDown
                    cmp cx,3 
					jz @@skipRight
					mov       [es:di+bp],al
@@skipRight:
					sub       di,bp
					cmp cx,4 
					jz @@skipLeft
                    mov       [es:di],al
@@skipLeft:					
@@skipDown:				   
   				    pop       di
                    ret 
endp _4pixels

				
proc _2pixels
					neg       si
                    push      di
                    add       di,si
					call  FindScreenOffset
					 
					cmp cx,2
					jz skipUp
					cmp cx,3 
					jz @@skipRight
                    mov       [es:di+bp],al
@@skipRight:
					sub       di,bp
					cmp cx,4 
					jz @@skipLeft
                    mov       [es:di],al
@@skipLeft:
skipUp:			
					 
					pop       di
                    ret 
endp _2pixels

proc FindScreenOffset
	push cx
	mov   cx,di    
	shl   di,6     
	shl   cx,8
	add   di,cx
	add   di,dx
	pop cx
	ret
endp 	FindScreenOffset

proc MoveRight
	push ax
	push bx
	push cx
	push dx
	
	mov ax, [spaceship_place]
	add ax, 20
	cmp ax, 215
	je @@end
	call undraw_player
	add [spaceship_place], 5
	mov [big_circle_color], 8
	mov [small_circle_color], 11
	call draw_player

@@end:
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp MoveRight

proc MoveLeft
	push ax
	push bx
	push cx
	push dx
	
	mov ax, [spaceship_place]
	sub ax, 20
	cmp ax, 0
	je @@end
	
	call undraw_player
	sub [spaceship_place], 5
	mov [big_circle_color], 8
	mov [small_circle_color], 11
	call draw_player

@@end:
	 pop dx
	 pop cx
	 pop bx
	 pop ax
ret
endp MoveLeft

proc movment
	push ax
	push bx
	push cx
	push dx 
	call CheckAndReadKey
	jz @@end
	cmp ah, 4bh
	je left
	cmp ah, 4dh
	je right
	jmp @@end
left:
	call Moveleft
	jmp @@end
right: 
	call MoveRight
@@end:
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp movment

; cx = col dx= row al = color si = height di = width 
proc draw_rect
	push si
	push di
	mov di, [rect_width]
	mov si, [rect_hight]
	call rect
	pop di
	pop si
ret
endp draw_rect


proc draw_row
	push ax
	push bx
	push cx
	push dx 
	push si
	
	mov ah, [row_num]
	dec ah
	mov [temp_row_num], ah
draw_all:
	mov [rect_col], 10
	mov bl, 10
	mov al,[temp_row_num]
	mul [row_mult]
	mov si, ax
	
	mov al, [temp_row_num]
	mul [y_mult]
	mov [rect_row], al
	
draw_loop:
	push bx
	mov al, [row_array + si]
	mov cx, [rect_col]
	push ax
	mov ax, 0
	mov al, [rect_row]
	mov dx, ax
	pop ax
	call draw_rect
	add [rect_col], 20
	
	inc si
	pop bx
	dec bl
	cmp bl, 0
	je @@cont
	jmp draw_loop
@@cont:
	cmp [temp_row_num], 0
	je @@end
	dec [temp_row_num]
	jmp draw_all
@@end:
	mov [rect_col], 10
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp draw_row

proc down_row
	
	call SetGraphic
	call draw_player
	add [rect_row], 27
	call draw_row
	mov ax, [word rect_row]
	mov bx, [spaceship_place]
	sub bx, 20
	cmp ax, bx
	jb @@cont
	mov [game_end], 1
@@cont:
ret
endp down_row


proc undraw_player
	mov [big_circle_color], 0
	mov [small_circle_color], 0
	call draw_player
	mov [big_circle_color], 8
	mov [small_circle_color], 11
ret
endp undraw_player

proc row_colors
	push ax
	push si
	push cx
	
	mov cx, 10
	mov si, 0
	mov al, 1
@@color_loop:
		mov [row_array + si], al
		inc al
		inc si
		loop @@color_loop
	pop cx
	pop si
	pop ax
	
ret
endp row_colors

proc erase_first_row
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov si, 0
	mov cx, 10
@@erase_loop:
	mov [row_array + si], 0
	inc si
	loop @@erase_loop
	
	
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp erase_first_row

proc push_row_down
	push ax
	push bx
	push cx
	push dx
	push si
	
	
	mov si, 49
	mov bx, 59
	mov al, 0
	mov cx, 50
@@down_loop:
	mov al, [row_array + si]
	mov [row_array + bx], al
	dec si
	dec bx
	loop @@down_loop
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp push_row_down


proc add_row
	push ax
	push si
	push cx
	push bx
	push dx
	
	call push_row_down
	cmp [row_array + 59], 0
	je @@cont
	mov [game_end], 1
@@cont:
	call erase_first_row
	mov si, 0
@@random_loop:
	mov bl, 1
	mov bh, 10
	call RandomByCs

	mov dx, bx
	mov bx, 0
	mov cx, 10
@@check_color_loop:
	cmp al, [row_array + bx]
	je @@random_loop
	inc bx
	loop @@check_color_loop
	mov bx, dx
	
	mov [row_array + si], al
	inc si
	cmp si, 10
	jne @@random_loop

	call SetGraphic
	call draw_screen
	call color_to_shoot ; remove
	inc [row_num]
	call draw_row
	
	pop dx
	pop bx
	pop cx
	pop si
	pop ax
ret
endp add_row


proc RandomByCs
    push es
    push si
    push di

    mov ax, 40h
    mov    es, ax

    sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
               ; Now bh holds only the delta
    cmp bh,0
    jz @@ExitP
 
    mov di, [word RndCurrentPos]
    call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)

RandLoop: ;  generate random number 
    mov ax, [es:06ch] ; read timer counter
    mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
    xor al, ah ; xor memory and counter

    ; Now inc di in order to get a different number next time
    inc di
    cmp di,(EndOfCsLbl - start - 1)
    jb @@Continue
    mov di, offset start
@@Continue:
    mov [word RndCurrentPos], di

    and ax, si ; filter result between 0 and si (the nask)
    cmp al,bh    ;do again if  above the delta
    ja RandLoop

    add al,bl  ; add the lower limit to the rnd num

@@ExitP:
    pop di
    pop si
    pop es
    ret
endp RandomByCs


Proc MakeMask
    push bx

    mov si,1

@@again:
    shr bh,1
    cmp bh,0
    jz @@EndProc

    shl si,1 ; add 1 to si at right
    inc si

    jmp @@again

@@EndProc:
    pop bx
    ret
endp  MakeMask

proc draw_seperate
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	mov cx, 215
	mov dx, 0
	mov al, 14
	mov si, 200
	mov di, 5
	call rect
	
	
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp draw_seperate

proc write_color
	push ax
	push bx
	push cx
	push dx
	push ds
	
	mov bh, 0
	mov dh, 1
	mov dl,28
	mov ah, 2
	int 10h
	
	
	mov dx, offset color_write
	mov ah, 9
	int 21h
	
	
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp write_color

; cx = col dx= row al = color si = height di = width 
proc color_to_shoot
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov bl, 1
	mov bh, 10
	call RandomByCs
	
	mov cx, 225
	mov dx, 20
	mov si, 25
	mov di, 90
	call rect
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp color_to_shoot

proc write_score
	push ax
	push bx
	push cx
	push dx
	push ds
	
	mov bh, 0
	mov dh, 7
	mov dl,28
	mov ah, 2
	int 10h
	
	
	mov dx, offset score_write
	mov ah, 9
	int 21h
	
	call write_the_score
	
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax

ret
endp write_score

proc undraw_row
	push ax
	push bx
	push cx
	push dx


	mov bx, 0
	mov bl, [row_num]
	dec bl
	mul bx
	mov bx, ax
	
	mov cx, 10
@@underaw_loop:
	mov [row_array + bx], ?
	inc bx
	loop @@underaw_loop

	call draw_row
	
	cmp [row_num], 0
	je @@end
	dec [row_num]
@@end:
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp undraw_row

proc draw_screen
	call draw_player
	call draw_seperate
	call write_color
	call write_score
	call draw_shot
ret 
endp draw_screen


proc score_to_string
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov ax,0
	mov [score_length], ax
	mov al, [score]
@@check_length:

		mov ah, 0
		div [row_mult]
		cmp al, 0
		je @@end_loop
		inc [score_length]
		jmp @@check_length
@@end_loop:
	inc [score_length]
	mov cx, [score_length]
	mov al, [score]
	mov si, [score_length]
	mov [score_to_write + si], '$'
	dec si
@@score_to_string_loop:
	mov bx, si
	cmp si, 0
	je @@cont
	dec bx
@@cont:
	mov ah, 0
	div [row_mult]
	add ah, 30h
	
	mov [score_to_write + si], ah
	dec si
	loop @@score_to_string_loop
	
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp score_to_string

proc write_the_score
	push ax
	push bx
	push cx
	push dx
	push ds
	
	call score_to_string
	
	mov bh, 0
	mov dh, 7
	mov dl,34
	mov ah, 2
	int 10h
	
	
	mov dx, offset score_to_write
	mov ah, 9
	int 21h
	
	
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
ret
endp write_the_score

proc draw_shot
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov bl, 36
	mov si, 0
	mov [shotY], 150
	push ax
	mov ax, [spaceship_place]
	mov [shotX], ax
	pop ax
@@shot_draw:
	push cx
	mov cx, [shotX]
	mov dx, [shotY]
	mov ah, 0ch
	mov al, [shot_matrix + si]
	int 10h
	pop cx
	inc si
	inc cx
	cmp cx, 6
	jne @@cont
	mov cx, 0
	inc [shotY]
	push ax
	mov ax, [spaceship_place]
	mov [shotX], ax
	pop ax
	dec [shotX]
@@cont:
	inc [shotX]
	cmp bl, 0
	je @@end
	dec bl
	jmp @@shot_draw

	
@@end:
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	
ret
endp draw_shot


	EndOfCsLbl:
END start


