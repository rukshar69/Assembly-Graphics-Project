.Model Small
draw_row Macro x ;macro for drawing a row
    Local l1
    ; draws a line in row x from col 0 to col 320
    MOV AH, 0CH
    MOV AL, 3
    MOV CX, 0
    MOV DX, x
L1: INT 10h
    INC CX
    CMP CX, 320
    JL L1
    EndM

draw_col Macro y ;macro for drawing a row
    Local l2
; draws a line col y from row 10 to row 189
    MOV AH, 0CH
    MOV AL, 1
    MOV CX, y
    MOV DX, 10
L2: INT 10h
    INC DX
    CMP DX, 190
    JL L2
    EndM

.Stack 100h
.Data
new_timer_vec   dw  ?,? ;variables for timer transitions
old_timer_vec   dw  ?,?

new_key_vec dw ?,?
old_key_vec dw ?,?

scan_code db 0
key_flag db 0
;paddle co-ords
paddle_top dw 70    ;player's cars initial position
paddle_bottom dw 120
paddle_left dw 0
paddle_right dw 30

timer_flag  db  0
vel_x       dw  -8 ;3 diff velocities for 3 enemy cars
vel_x2       dw   -9
vel_x3  dw -5

;for display ball method
init_pos_row dw ? ;variables for drawing balls
init_pos_col dw ? 
limit_for_row dw ?

;saving the rows, cols
b1_r dw ?
b1_c dw ?
b2_r dw ?
b2_c dw ?
b3_r dw ?
b3_c dw ?
;initial row,cols

init_b1_r dw 70
init_b1_r_end dw 120

init_b_c dw 297 ;initial col same for all balls


init_b2_r dw 10
init_b2_r_end dw 60

init_b3_r dw 140
init_b3_r_end dw 190
;initial paddle

init_pad_r dw ?
init_pad_c dw ?
;paddle co ords

pad_r dw ?
pad_c dw ?

;score saver
score dw 0

;scan codes
up_arrow = 72
down_arrow = 80
esc_key  = 1
;for display of score
TENS_COMPARE DW 10000D 
ANS DW 0
CHAR DB 0 
.Code

set_display_mode Proc
; sets display mode and draws boundary
    MOV AH, 0
    MOV AL, 04h; 320x200 4 color
    INT 10h
    
    MOV AH, 0BH
   
; set bgd color
    MOV BH, 0
    MOV BL, 0; black
    INT 10h
    
    ;select palette 1- cyan , 2- red, 3 - white
     MOV BH, 1
    MOV BL, 1
    INT 10h
; draw boundary
    draw_row 66
    draw_row 133
    ;draw_col 10
    ;draw_col 300
    
    RET
set_display_mode EndP

;the method sets up timer interrupt for ball moving after each sec of timer tick
;otherwise balls move very fast
;in order to get out of the timer circuit interrupt we need to move from one vector to the other
; this is done through this method
setup_int Proc
; save old vector and set up new vector
; input: al = interrupt number
;    di = address of buffer for old vector
;    si = address of buffer containing new vector
; save old interrupt vector
    MOV AH, 35h ; get vector
    INT 21h
    MOV [DI], BX    ; save offset
    MOV [DI+2], ES  ; save segment
; setup new vector
    MOV DX, [SI]    ; dx has offset
    PUSH DS     ; save ds
    MOV DS, [SI+2]  ; ds has the segment number
    MOV AH, 25h ; set vector
    INT 21h
    POP DS
    RET
setup_int EndP


;for consistent delay time
;each time its activated the timer_flag turns 1 to do the ball moving
;after each activatio the timer_flag becomes 0
;the method tells us what should be done after each tick i.e. making timer_flag 1
timer_tick Proc
    PUSH DS
    PUSH AX
    
    MOV AX, Seg timer_flag
    MOV DS, AX
    MOV timer_flag, 1
    
    POP AX
    POP DS
    
    IRET
timer_tick EndP

;;;;;;;;;;;;;;;;;first ball method;;;;;;;;;;;;;;
display_ball Proc
; displays ball at col CX and row DX with color given in AL
; input: AL = color of ball
;    CX = col
;    DX = row
    mov init_pos_row,dx ;saving the initial positions
    mov init_pos_col,cx
    MOV AH, 0CH ; write pixel
    
    add dx,50 ;determine limit for row
    mov limit_for_row, dx ;saving it 
    sub dx, 50 ; returning to initial row
    
    w_row: ;beginning of loop for each row drawing
    
    cmp dx, limit_for_row ;present row == limit
    je end_w_row ;if, yes end of loop
    mov bx , 1 ;inner loop counter
    w1:
        cmp bx,50 ;loop for 50 times == 0 cols are drawn
        je end_w1
        dec cx ;move to next column
        int 10h
        inc bx
        jmp w1
        end_w1:
        
    
    mov cx,init_pos_col ;saving the position of column
    
    INC DX      ; down 1 row
    jmp w_row
    end_w_row:
    
    
    mov dx,init_pos_row ;getting back the init pos
    mov cx,init_pos_col
    
    RET 
display_ball EndP

move_ball Proc
; erase ball at current position and display at new position
; input: CX = col of ball position
;    DX = rwo of ball position
; erase ball
    MOV AL, 0
    CALL display_ball
; get new position
    ADD CX, vel_x
    mov b1_c, cx ;saving the current ball col pos
    ;ADD DX, vel_y
; check boundary
    CALL check_boundary
; wait for 1 timer tick to display ball
test_timer:
    CMP timer_flag, 1
    JNE test_timer
    MOV timer_flag, 0
    MOV AL, 3
    CALL display_ball
    RET 
move_ball EndP

check_boundary Proc
; determine if ball is outside screen, if so move it back in and 
; change ball direction
; input: CX = col of ball
;    DX = row of ball
; output: CX = valid col of ball
;     DX = valid row of ball
; check col value
    mov bx, cx
    sub bx,50 ; moving to the front point of enemy car
    cmp bx, 30 ; checking its boundary
    jg done
    
    cmp dx,paddle_top ;if the ball in same lane
    jne check ;no, go to normal checking
    call game_over ;yes game over
    check:
    CMP bX, 11
    JG done
    
    xor ax,ax ; clear ax
    
    MOV AL, 0 ;vanish the ball
    CALL display_ball
   
   ;;initial position
   
   inc score
   mov dx, init_b_c ;moving the ball in the initial position and saving its pos
    mov b1_c, dx
    
    
     mov dx, init_b1_r
     mov b1_r, dx  
     mov cx, init_b_c
     mov dx, init_b1_r
   

done:
    RET 
check_boundary EndP
;;;;;;;;;;;;;;;;;;second ball methods;;;;;;;;;;;;;;;

display_ball2 Proc
; displays ball at col CX and row DX with color given in AL
; input: AL = color of ball
;    CX = col
;    DX = row
    mov init_pos_row,dx
    mov init_pos_col,cx
    MOV AH, 0CH ; write pixel
    
    add dx,50
    mov limit_for_row, dx
    sub dx, 50
    
    w2_row:
    
    cmp dx, limit_for_row
    je end_w2_row
    mov bx , 1
    w12:
        cmp bx,50
        je end_w12
        dec cx
        int 10h
        inc bx
        jmp w12
        end_w12:
        
    
    mov cx,init_pos_col
    
    INC DX      ; down 1 row
    jmp w2_row
    end_w2_row:
    
    
    mov dx,init_pos_row
    mov cx,init_pos_col
    
    RET 
    display_ball2 EndP
    
move_ball2 Proc
; erase ball at current position and display at new position
; input: CX = col of ball position
;    DX = rwo of ball position
; erase ball
    MOV AL, 0
    CALL display_ball2
; get new position
    ADD CX, vel_x2
    mov b2_c, cx
    ;ADD DX, vel_y
; check boundary
    CALL check_boundary2
; wait for 1 timer tick to display ball
test_timer2:
    CMP timer_flag, 1
    JNE test_timer2
    MOV timer_flag, 0
    MOV AL, 3
    CALL display_ball2
    RET 
move_ball2 EndP

check_boundary2 Proc
; determine if ball is outside screen, if so move it back in and 
; change ball direction
; input: CX = col of ball
;    DX = row of ball
; output: CX = valid col of ball
;     DX = valid row of ball
; check col value
    mov bx, cx
    sub bx,50
    cmp bx, 30
    jg done2
    
    cmp dx,paddle_top
    jne check2
    call game_over
    check2:
    
    
    CMP bX, 11
    JG done2
    
    MOV AL, 0
    CALL display_ball2
   ; mov cx, 297
   ;mov dx, 70
   
   ;;initial position
   
   inc score
   
   mov dx, init_b_c
   mov b2_c, dx
    
    
   mov dx, init_b2_r
   mov b2_r, dx  
     mov cx, init_b_c
     mov dx, init_b2_r
     
   

     done2:
    RET 
    check_boundary2 EndP


;;;;;;;;;;;;;;;;;third ball method;;;;;;;;;;;;;;
display_ball3 Proc
; displays ball at col CX and row DX with color given in AL
; input: AL = color of ball
;    CX = col
;    DX = row
    mov init_pos_row,dx
    mov init_pos_col,cx
    MOV AH, 0CH ; write pixel
    
    add dx,50
    mov limit_for_row, dx
    sub dx, 50
    
    w3_row:
    
    cmp dx, limit_for_row
    je end_w3_row
    mov bx , 1
    w13:
        cmp bx,50
        je end_w13
        dec cx
        int 10h
        inc bx
        jmp w13
        end_w13:
        
    
    mov cx,init_pos_col
    
    INC DX      ; down 1 row
    jmp w3_row
    end_w3_row:
    
    
    mov dx,init_pos_row
    mov cx,init_pos_col
    
    RET 
    display_ball3 EndP

move_ball3 Proc
; erase ball at current position and display at new position
; input: CX = col of ball position
;    DX = rwo of ball position
; erase ball
    MOV AL, 0
    CALL display_ball3
; get new position
    ADD CX, vel_x3
    mov b3_c, cx
    ;ADD DX, vel_y
; check boundary
    CALL check_boundary3
; wait for 1 timer tick to display ball
test_timer3:
    CMP timer_flag, 1
    JNE test_timer3
    MOV timer_flag, 0
    MOV AL, 3
    CALL display_ball3
    RET 
    move_ball3 EndP

check_boundary3 Proc
; determine if ball is outside screen, if so move it back in and 
; change ball direction
; input: CX = col of ball
;    DX = row of ball
; output: CX = valid col of ball
;     DX = valid row of ball
; check col value
    mov bx, cx
    sub bx,50
    cmp bx, 30
    jg done3
    
    cmp dx,paddle_top
    jne check3
    call game_over
    check3:
    
    CMP bX, 11
    JG done3
    
    MOV AL, 0
    CALL display_ball3
   ; mov cx, 297
   ;mov dx, 70
   
   
   ;;initial position
   
   inc score
   
   mov dx, init_b_c
   mov b3_c, dx
    
    
   mov dx, init_b3_r
   mov b3_r, dx  
     mov cx, init_b_c
     mov dx, init_b3_r
   

  done3:
    RET 
    check_boundary3 EndP

    ;;;;;;;;;;;;paddle method;;;;;;;;
    
    ;since we don't know when a key is pressed we use key interrupt -- interrupt 9 the keyboard interrupt
    ;it'll access the i/o ports directly and the scan code
    ;There are three 1/0 ports to be accessed. Wheri the keyboard generates
    ;an Interrupt, bit S of the I/0 port 20h is set, and the scan code comes
    ;into port 60h. Port 61 h, bit 7, is used to enable the keyboard. Therefore, the.
    ;interrupt routine should read the data from port 60h, enable the keyboard
    ;by setting bit 7 in port 61h, and clear bit 5 of port 20h.
    
    
    ;The interrupt procedure is called l<EYBOARD INT. When it obtaim
    ;a scan ~ode, it first checks to see if the scan code is a make or break code.
    ;If it finds a make code, it sets the variable ?KEV_FLAG and puts the make
    ;code in the variable SCAN_ CODE. If it finds a break code, the variables are
    ;not changed.
keyboard_int proc

    push ds
    push ax
    mov ax, seg scan_code ;getting the scan_code ready for using it in interrupts
    mov ds,ax
    
    in al,60h ;read scan code
    push ax ;saving it
    in al,61h ;control port value
    mov ah,al ;save it in ah
    or al, 80h ;set bit for keyboard
    out 61h,al ;write back
    xchg ah,al ;get back control value
    out 61h, al ;reset control port
    pop ax ;recover scan code
    mov ah,al ;save scan code in ah
    test al, 80h ;test for break code
    jne key_0 ;yes, clear flags goto key_0
    
    ;make code
    mov scan_code, al ;save in variable
    mov key_flag,1 ;set key flag
    key_0: ;reset interrupt
        mov al,20h
        out 20h, al
        
        pop ax
        pop ds
        
        iret
keyboard_int endp

draw_paddle proc
;saving registers
    push cx
    push  dx
    push bx
    ;mov ah, 0ch 
    ;mov cx, 30
    ;mov dx, paddle_top ;start from paddle_top
    
    ;write pixel function
    mov ah, 0ch
    mov cx, 30
    mov dx, paddle_top ;top row
    
    dp1: ;traverse each row
        int 10h
        ;dec cx
        ;int 10h
        
        mov bx, 1 ;the breadth of player 30px
        c:
            cmp bx, 29
            je end_c
            dec cx
            int 10h
            inc bx
            jmp c
            end_c:
        mov cx,30
        inc dx
        
        cmp dx, paddle_bottom
        jle dp1
    pop bx    
    pop dx
    pop cx
    
    ret 
draw_paddle endp

move_paddle proc
    
    ;ax has the paddle_top
    mov bx,ax
    
        
    ;update:
    mov al,0 ;vanish the ol' pos
        call draw_paddle
        mov paddle_top, bx
        add bx, 50
        mov paddle_bottom, bx
        
        mov al, 1
        call draw_paddle
    done1:
        ret
move_paddle endp



main Proc
    MOV AX, @data
    MOV DS, AX
    

    CALL set_display_mode
    
    
    mov al,1
    call draw_paddle
    
    
    
    
    MOV new_timer_vec, offset timer_tick
    MOV new_timer_vec+2, CS
    MOV AL, 1CH; interrupt type
    LEA DI, old_timer_vec
    LEA SI, new_timer_vec
    CALL setup_int
    
    MOV NEW_KEY_VEC,OFFSET KEYBOARD_INT ;offset
    mov new_key_vec+2,CS ; Se91"ent
    MOV al, 9h;interrupt number
    LEA di, old_key_vec
    LEA si, new_key_vec
    CALL setup_int
   
   ;first ball 
   MOV CX, init_b_c
   MOV DX, init_b1_r
    MOV AL, 3
    CALL display_ball
    mov dx, init_b_c
    mov b1_c, dx
    
     mov dx, init_b1_r
     mov b1_r, dx   
   
   ;second ball
    MOV CX, init_b_c
    MOV DX, init_b2_r
    MOV AL, 3
    CALL display_ball2
    mov dx, init_b_c
    mov b2_c, dx
    
    mov dx, init_b2_r
     mov b2_r, dx   
   
     ;third ball
    MOV CX, init_b_c
    MOV DX, init_b3_r
    MOV AL, 3
    CALL display_ball3
    mov dx, init_b_c
    mov b3_c, dx
    
    mov dx, init_b3_r
    mov b3_r, dx 
  
    
    test_key:
    cmp key_flag,1;check key flag
    jne tt  ;not set, clr it and check
    mov key_flag,0 ;flag set,clr it and check
    cmp scan_code, esc_key ;esc, terminate 
    jne tk_1 ;no, check arrow keys
    jmp doneMain ;esc,terminate
        tk_1:
            cmp scan_code, up_arrow
            jne tk_2 ;no , check down arrow
            mov ax, paddle_top
            cmp ax, 10
            jne check_second_lane
        
            call move_paddle;can't go up
            
            jmp tt ;go to test_timer
            
            check_second_lane:
                cmp ax,70
                jne check_third_lane
                mov ax, 10 ;going up to 1st lane
                
                call move_paddle
            
                jmp tt
            check_third_lane:
                cmp ax,140
                jne tt
                mov ax, 70 ;goint to 2nd lane
                call move_paddle
            
                jmp tt
                
        tk_2:
            cmp scan_code, down_arrow
            jne tt ;some other button
            mov ax, paddle_top
            cmp ax, 10 ;if 1st lane
            jne check_second_lane_down
            
            mov ax, 70 ;move to 2nd lane
            call move_paddle
            
            jmp tt
            
            check_second_lane_down:
                cmp ax,70
                jne check_third_lane_down
                mov ax, 140 ;going to 3rd lane
                
                call move_paddle
            
                jmp tt
            check_third_lane_down:
            cmp ax,140 ;can't go further down
                jne tt
                
                call move_paddle
            
; wait for timer tick before moving the ball  
    
    tt:
    CMP timer_flag, 1;flag set?
    JNE tt;no keep checking
    MOV timer_flag, 0 ;yes, clr it
    
    ; ball pos
    mov dx,b1_r
    mov cx, b1_c
    
    CALL move_ball
  
    ;ball2 pos
    mov dx,b2_r
    mov cx, b2_c
    
    CALL move_ball2
    
    ;ball3 pos
    mov dx,b3_r
    mov cx, b3_c
    
    CALL move_ball3
    
    ;paddle check
    mov al,1
   
    call draw_paddle
    JMP test_key

doneMain:
call game_over    
   
main EndP
game_over proc
lea di,new_timer_vec ;reset the int vectors
    lea si, old_timer_vec
    mov al , 1ch
    call setup_int
    
    lea di,new_key_vec
    lea si, old_key_vec
    mov al , 9h
    call setup_int
    
    mov ah,0 ;read key
    int 16h
    
    ;return to txt mode
    mov ah ,0 ;wait for input
    mov al, 3
    int 10h
    
   ; mov ah, 2
   ;mov dx ,score
   ;int 21h
    
    mov ax, score
    mov ans,ax
    
    cmp ax,0
    je print_zero
      
     WHILE_2_START:
        
        MOV AX,TENS_COMPARE
        CMP AX,ANS
        JLE WHILE_2_END
        
        MOV AX,TENS_COMPARE
        MOV DX,0D
        MOV BX,10D
        IDIV BX
        MOV TENS_COMPARE,AX
        
        
        JMP WHILE_2_START
    WHILE_2_END:
    
    
    
    
    WHILE_3_START:
        CMP TENS_COMPARE,0D
        JLE WHILE_3_END
        
        MOV DX,0D 
        MOV AX,ANS
        MOV BX,TENS_COMPARE
        IDIV BX
        MOV ANS,DX
        MOV CHAR,AL
        ADD CHAR,'0'
        
        MOV AH,2
        MOV DL,CHAR
        INT 21H
        
        MOV AX,TENS_COMPARE
        MOV DX,0D
        MOV BX,10D
        IDIV BX
        MOV TENS_COMPARE,AX
        
        
        
        JMP WHILE_3_START
    WHILE_3_END:
    jmp D
    
    print_zero:
        mov ah,2
        mov dl, '0'
        int 21h
        jmp D
    
    ;return to dos
    
    D:
    mov ah, 4ch
    int 21h
    
    ret
    game_over endp
End main

