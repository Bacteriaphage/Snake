DAT_R  EQU P1.0
DAT_L  EQU P1.1
SCK    EQU P1.2
OE     EQU P1.3
RCK    EQU P1.4
ARRAY  EQU  30H
TIME   DATA 26H
BEANP  DATA 27H
TEMP1  DATA 28H
TEMP2  DATA 29H
LINE   EQU  50H

ORG 0000H
AJMP MAIN

ORG 0003H
AJMP INTE0

ORG 000BH
AJMP IN0

ORG 0013H
AJMP INTE1

ORG 001BH
AJMP IN1

ORG 0100H
MAIN:
        MOV     SP,     #70H                ;INITIALIZE
        SETB    20H.1
        CLR     20H.0
        MOV     TEMP1,  #00H
        MOV     TEMP2,  #00H
        MOV     TIME,   #14
        MOV     TMOD,   #11H
        MOV     TH0,    #00H
        MOV     TL0,    #00H
        MOV     TH1,    #00H
        MOV     TL1,    #00H
        SETB    TR0
        SETB    TR1
        SETB    ET0
        SETB    ET1
        SETB    PT1
        SETB    IT0
        SETB    IT1
        CLR     PT0
        SETB    EX1
        SETB    EX0  
        SETB    EA
        ACALL   BEAN
        ACALL   PRE_TRA


;********************************************************************************************************
;                
;       将数据显示在屏幕上      
;
;********************************************************************************************************
DISPLAY:
        CLR    OE
        MOV    R0,      #00H
        MOV    R6,      #10H              ;循环16次，点亮所有行
READ:                                    ;从外部寄存器中读取字模
        MOV    A,       R0               ;读取行数据
        MOV    DPTR,    #ROW       
        MOVC   A,       @A+DPTR          ;读取低位
        MOV    R2,      A
        INC    R0
        MOV    A,       R0               ;读取高位
        MOVC   A,       @A+DPTR
        MOV    R3,      A
        DEC    R0                         ;读取列数据
DIS_C_DO: 
        MOV    A,       #LINE             ;USE ORL TO CHANGE DATA BLOCK
        ADD    A,       R0 
        MOV    R1,      A
        MOV    A,       @R1              ;读取低位
        MOV    R4,      A
        INC    R0
        INC    R1
        MOV    A,       @R1              ;读取高位  
        MOV    R5,      A
        INC    R0
        MOV    R1,      #08H             ;准备按位送数据
        CLR    RCK

SHIFTHIGH:                               ;将行与列的高位填入移位寄存器
        CLR    SCK
        MOV    A,       R5
        RLC    A
        MOV    DAT_R,   C
        CLR    C
        MOV    R5,      A
        MOV    A,       R3
        RLC    A
        MOV    DAT_L,   C
        CLR    C
        MOV    R3,      A
        SETB   SCK
        DJNZ   R1,      SHIFTHIGH
        MOV    R1,      #08H

SHIFTLOW:                                 ;将行与列的低位填入移位寄存器
        CLR    SCK
        MOV    A,       R4
        RLC    A
        MOV    DAT_R,   C
        CLR    C
        MOV    R4,      A
        MOV    A,       R2
        RLC    A
        MOV    DAT_L,   C
        CLR    C
        MOV    R2,      A
        SETB   SCK
        DJNZ   R1,      SHIFTLOW

SHOW:                                     ;在点阵上显示
        SETB   RCK                        ;将数据锁存
        ACALL  DELAY
        DJNZ   R6,      READ              ;循环16次
        MOV    R6,      #10H              ;再次初始化
        MOV    R0,      #00H
        SJMP   DISPLAY
DIS_END:

;*******************************************************************************************************************
;
;       贪吃蛇移动
;
;*******************************************************************************************************************
SNAKEMOVE:
        CLR    EX0
        CLR    EX1
        MOV    R6,      TEMP1
        MOV    R7,      TEMP2
SNA_CHECK1:
        MOV    R7,      TEMP2
        MOV    A,       #ARRAY
        ADD    A,       R7
        MOV    R1,      A
        MOV    A,       @R1
        MOV    R4,      A
        CLR    C
        JNB    20H.0,   SNA_VERTICAL
        JNB    20H.1,   SNA_HO_UP
SNA_HO_DOWN:
        ORL    A,       #0F0H
        CPL    A
        JZ     GAMEOVER
        MOV    A,       R4
        ADD    A,       #01H
        MOV    R5,      A
        SJMP   SNA_CHECK2
SNA_HO_UP:
        ANL    A,       #0FH
        JZ     GAMEOVER
        MOV    A,       R4
        SUBB   A,       #01H
        MOV    R5,      A
        SJMP   SNA_CHECK2
SNA_VERTICAL:
        JNB    20H.1,  SNA_VE_LEFT
SNA_VE_RIGHT:
        ORL    A,       #0FH
        CPL    A
        JZ     GAMEOVER
        MOV    A,       R4
        ADD    A,       #10H
        MOV    R5,      A
        SJMP   SNA_CHECK2
SNA_VE_LEFT:
        ANL    A,       #0F0H
        JZ     GAMEOVER
        MOV    A,       R4
        SUBB   A,       #10H
        MOV    R5,      A
SNA_CHECK2:
        MOV    R7,      TEMP2
        MOV    R6,      TEMP1
        CLR    C
        MOV    A,       R7
        SUBB   A,       R6
        JC     SNA_CH2_ABNORMAL
SNA_CH2_NORMAL:
        MOV    R6,      TEMP1                           
        MOV    R4,      A
        INC    R4
        MOV    A,       #ARRAY
        ADD    A,       R6
        MOV    R0,      A
SNA_CH2_NO_LOOP:
        MOV    A,       @R0
        MOV    R1,      A
        MOV    A,       R5
        XRL    A,       R1
        JZ     GAMEOVER
        INC    R0
        DJNZ   R4,      SNA_CH2_NO_LOOP
        SJMP   SNA_BEAN
SNA_CH2_ABNORMAL:
        MOV    R7,      TEMP2
        MOV    R6,      TEMP1
        MOV    A,       R6
        SUBB   A,       R7
        MOV    R4,      A
        MOV    A,       #32
        SUBB   A,       R4
        MOV    R4,      A
        INC    R4
        MOV    A,       #ARRAY
        MOV    R6,      TEMP1
        ADD    A,       R6
        MOV    R0,      A
SNA_CH2_AB_LOOP:
        MOV    A,       @R0
        MOV    R1,      A
        MOV    A,       R5
        XRL    A,       R1
        JZ     GAMEOVER
        MOV    A,       #50H
        SUBB   A,       R0
        JZ     SNA_CH2_EXCEED
        INC    R0
        DJNZ   R4,      SNA_CH2_AB_LOOP
        SJMP   SNA_BEAN
SNA_CH2_EXCEED:
        MOV    R0,      #ARRAY
        DJNZ   R4,      SNA_CH2_AB_LOOP
GAMEOVER:
        LCALL  OVER 
SNA_BEAN:
        MOV    A,       BEANP
        SUBB   A,       R5
        JNZ    SNA_MOVE
SNA_HIT:
        MOV    R7,      TEMP2
        ACALL  BEAN
        INC    R7
        MOV    A,       R7
        MOV    B,       #32
        DIV    AB
        MOV    R7,      B
SNA_HIT_MO:
        MOV    A,       #ARRAY
        ADD    A,       R7
        MOV    R0,      A 
        MOV    A,       R5
        MOV    @R0,     A
        SJMP   SNA_END                       
SNA_MOVE:
        MOV    R7,      TEMP2
;        MOV    A,       #32
;        SUBB   A,       R7
;        JZ     SNA_MO_EXCEED
        INC    R7
        MOV    A,       R7
        MOV    B,       #32
        DIV    AB
        MOV    R7,      B
        MOV    A,       #ARRAY
        ADD    A,       R7
        SJMP   SNA_MO_WRITE
SNA_MO_WRITE:
        MOV    R0,      A
        MOV    A,       R5
        MOV    @R0,     A
SNA_MOVE2:
        MOV    R6,      TEMP1        
        MOV    A,       #ARRAY
        ADD    A,       R6
        MOV    R0,      A
        MOV    A,       @R0
SNA_SENDA:                                  ;SEND LOW HALF BYTE OF ARRAY TO R2
        CLR    C
        ANL    A,       #0FH
        MOV    R2,      A
        MOV    R3,      #5H
        CLR    C
        MOV    A,       @R0
SNA_SENDB:                                  ;SEND HIGH HALF BYTE OF ARRAY TO R1
        RLC    A
        DJNZ   R3,      SNA_SENDB
        ANL    A,       #0FH
        MOV    R1,      A
SNA_CONVER:                                 ;SEND THE ADDRESS OF TARGET DATA INTO R1
        MOV    A,       R1                                 
        MOV    B,       #2H
        MUL    AB
        MOV    R1,      A
SNA_C_DO:
        MOV    R6,      TEMP1 
        MOV    A,       #LINE               ;USE ORL TO CHANGE DATA BLOCK
        ADD    A,       R1
        MOV    R1,      A
        MOV    @R1,     #00H           
        INC    R1
        MOV    @R1,     #00H
        MOV    A,       #ARRAY
        ADD    A,       R6
        MOV    R0,      A
        MOV    @R0,     #00H
        MOV    R6,      TEMP1
        INC    R6
        MOV    A,        R6
        MOV    B,        #32
        DIV    AB
        MOV    R6,       B
SNA_END:
        SETB   EX1
        SETB   EX0
        MOV    TEMP1,   R6
        MOV    TEMP2,   R7
        AJMP   PRE_TRA
;****************************************************************************************************        
;        
;      遍历数组中的数据并转换到显示数据块上  
;        
;**************************************************************************************************** 
PRE_TRA:  
        MOV     R5,     TEMP1
        MOV     R7,     TEMP2    
TRAVERSE:                                   ;TRAVERSE
        MOV     A,      #ARRAY
        ADD     A,      R5
        MOV     R0,     A
        MOV     A,      @R0
TRA_SENDA:                                  ;SEND LOW HALF BYTE OF ARRAY TO R2
        CLR     C
        ANL     A,      #0FH
        MOV     R2,     A
        MOV     R3,     #5H
        CLR     C
        MOV     A,      @R0
TRA_SENDB:                                  ;SEND HIGH HALF BYTE OF ARRAY TO R1
        RLC     A
        DJNZ    R3,     TRA_SENDB
        ANL     A,      #0FH
        MOV     R1,     A
TRA_CONVER:                                 ;SEND THE ADDRESS OF TARGET DATA INTO R1
        MOV     A,      R1                                 
        MOV     B,      #2H
        MUL     AB
        MOV     R1,     A 
        
        MOV     R3,     #01H
        MOV     R4,     #00H  
        CLR     C    
        CJNE    R2,#0,   TRA_C_MOVE
        SJMP    TRA_C_DO
TRA_C_MOVE:                                 ;GENERATE NEW DATA IN REGISTER R4 AND R3
        MOV     A,      R3
        RLC     A
        MOV     R3,     A
        MOV     A,      R4
        RLC     A
        MOV     R4,     A
        DJNZ    R2,     TRA_C_MOVE
TRA_C_DO: 
        MOV     A,      #LINE               ;USE ORL TO CHANGE DATA BLOCK
        ADD     A,      R1
        MOV     R1,     A
        MOV     A,      @R1
        ORL     A,      R3           
        MOV     @R1,    A
        INC     R1
        MOV     A,      @R1
        ORL     A,      R4
        MOV     @R1,    A
TRA_FINAL:                                  ;TO JUDGE IF CODE TRAVERSE ALL DATA IN THIS ARRAY
        CLR     C
        MOV     A,      R5
        SUBB    A,      R7
        JNZ     TRA_F_NEXT
        SJMP    TRA_END
TRA_F_NEXT:                                 ;TO JUDGE F CODE TRAVERSE THE LAST SPACE OF THE ARRAY
        INC     R5
        MOV     A,      R5
        MOV     B,      #32
        DIV     AB
        MOV     R5,     B
        SJMP    TRAVERSE
TRA_END:
        MOV     R1,     BEANP
        MOV     A,      R1
TRA_B_SENDA:                                  ;SEND LOW HALF BYTE OF ARRAY TO R2
        CLR     C
        ANL     A,      #0FH
        MOV     R2,     A
        MOV     R3,     #5H
        CLR     C
        MOV     A,      R1
TRA_B_SENDB:                                  ;SEND HIGH HALF BYTE OF ARRAY TO R1
        RLC     A
        DJNZ    R3,     TRA_B_SENDB
        ANL     A,      #0FH
        MOV     R1,     A
TRA_B_CONVER:                                 ;SEND THE ADDRESS OF TARGET DATA INTO R1
        MOV     A,      R1                                 
        MOV     B,      #2H
        MUL     AB
        MOV     R1,     A 
        
        MOV     R3,     #01H
        MOV     R4,     #00H  
        CLR     C    
        CJNE    R2,#0,   TRA_B_C_MOVE
        SJMP    TRA_B_C_DO
TRA_B_C_MOVE:                                 ;GENERATE NEW DATA IN REGISTER R4 AND R3
        MOV     A,      R3
        RLC     A
        MOV     R3,     A
        MOV     A,      R4
        RLC     A
        MOV     R4,     A
        DJNZ    R2,     TRA_B_C_MOVE
TRA_B_C_DO: 
        MOV     A,      #LINE               ;USE ORL TO CHANGE DATA BLOCK
        ADD     A,      R1
        MOV     R1,     A
        MOV     A,      @R1
        ORL     A,      R3           
        MOV     @R1,    A
        INC     R1
        MOV     A,      @R1
        ORL     A,      R4
        MOV     @R1,    A
        RET
;*****************************************************************************************************
;
;       取随机数生成豆子
;
;*****************************************************************************************************
      
BEAN:
        MOV     R1,     TL0
        MOV     BEANP,  R1
        MOV     A,      R1
BEAN_SENDA:                                  ;SEND LOW HALF BYTE OF ARRAY TO R2
        CLR     C
        ANL     A,      #0FH
        MOV     R2,     A
        MOV     R3,     #5H
        CLR     C
        MOV     A,      R1
BEAN_SENDB:                                  ;SEND HIGH HALF BYTE OF ARRAY TO R1
        RLC     A
        DJNZ    R3,     BEAN_SENDB
        ANL     A,      #0FH
        MOV     R1,     A
BEAN_CONVER:                                 ;SEND THE ADDRESS OF TARGET DATA INTO R1
        MOV     A,      R1                                 
        MOV     B,      #2H
        MUL     AB
        MOV     R1,     A 
        
        MOV     R3,     #01H
        MOV     R4,     #00H    
        CLR     C    
        CJNE    R2,#0,   BEAN_C_MOVE
        SJMP    BEAN_C_DO
BEAN_C_MOVE:                                 ;GENERATE NEW DATA IN REGISTER R4 AND R3
        MOV     A,      R3
        RLC     A
        MOV     R3,     A
        MOV     A,      R4
        RLC     A
        MOV     R4,     A
        DJNZ    R2,     BEAN_C_MOVE
BEAN_C_DO: 
        MOV     A,      #LINE               ;USE ORL TO CHANGE DATA BLOCK
        ADD     A,      R1
        MOV     R1,     A
        MOV     A,      @R1
        ORL     A,      R3           
        MOV     @R1,    A
        INC     R1
        MOV     A,      @R1
        ORL     A,      R4
        MOV     @R1,    A    
BEAN_END:
        RET
;******************************************************************************************************************                
;        
;       按键中断以及定时器中断       
;
;******************************************************************************************************************
INTE0:
        MOV     C,      20H.0
        CPL     C
        MOV     20H.0,  C
        MOV     C,      20H.1        
        CLR     C
        MOV     20H.1,  C
        ACALL   DELAYB
        RETI


INTE1:
        MOV     C,      20H.0
        CPL     C
        MOV     20H.0,  C
        MOV     C,      20H.1        
        SETB    C
        MOV     20H.1,  C
        ACALL   DELAYB
        RETI

IN0: 
        CLR     TR0
        MOV     TH0,     #00H
        MOV     TL0,     #00H
        SETB    TR0
        RETI


IN1:
        CLR     TR1
        MOV     TH1,     #00H
        MOV     TL1,     #00H
        MOV     R7,       TIME
        DEC     R7
        CJNE    R7,#0,    PASS
        MOV     TIME,    #14
        SETB    TR1
        ACALL   SNAKEMOVE
        RETI
PASS:   
        MOV     TIME,    R7
        SETB    TR1
        RETI



DELAY:
        MOV     R2,      #10H
DELAY0:
        MOV     R3,      #50H
DELAY1:
        DJNZ    R3,      DELAY1
        DJNZ    R2,      DELAY0
        RET


DELAYB:
        MOV     R2,      #030H 
DELAYB0:
        DJNZ    R2,      DELAYB0
        RET             
OVER:        
ROW:
DB      0FEH,0FFH,0FDH,0FFH,0FBH,0FFH,0F7H,0FFH,0EFH,0FFH,0DFH,0FFH,0BFH,0FFH,07FH,0FFH
DB      0FFH,0FEH,0FFH,0FDH,0FFH,0FBH,0FFH,0F7H,0FFH,0EFH,0FFH,0DFH,0FFH,0BFH,0FFH,07FH  
;
END
