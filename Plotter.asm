    org 100h
    jmp Start

include 'macro/proc16.inc'
include 'StringUtils.inc'
include 'RPN.inc'
include 'ConsoleIO.inc'

REGULAR_PLOT = 1
PARAMETRIC_PLOT = 2


SCREEN_WIDTH = 320
SCREEN_HEIGHT = 200

MAX_FUNCTIONS_COUNT = 3

FIRST_PLOT_COLOR = 52
AXES_COLOR = 0Fh

FIRST_POINT = 0
VALUE_UNDEFINED = 1
Y_OUT_OF_BOUNDS = 2
VALUE_OK = 3
X_OUT_OF_BOUNDS = 4


KEY_LEFT = 75
KEY_RIGHT = 77
KEY_UP = 72
KEY_DOWN = 80
KEY_ALT_X = 45
KEY_CTRL_PLUS = 144
KEY_CTRL_MINUS = 142
KEY_ESC = $1B

DIR_RIGHT = 1
DIR_LEFT = 2
DIR_UP = 4
DIR_DOWN = 8

ROUND_ZERO_MASK = 110000000000b


Start:
    stdcall PrintString, ChoosePlotType
    .ChoosePlotTypeLoop:
        stdcall InputString, StrBuffer
        mov al, [StrBuffer + 2]
        sub al, '0'
        cmp al, REGULAR_PLOT
        je @F
        cmp al, PARAMETRIC_PLOT
        je @F

        stdcall PrintString, InvalidPlotType
        stdcall PrintString, Newline
        jmp .ChoosePlotTypeLoop

    @@:
    mov [PlotType], al
    cmp al, PARAMETRIC_PLOT
    je .InputParametric

    stdcall PrintString, InputFunction
    mov bx, FunctionsRPNs
    .InputFunctionsLoop:
        stdcall InputString, StrBuffer
        add dx, 2
        push cx
        stdcall ToRPN, dx, bx, 'x'
        pop cx
        jc .InputNextFunction

        cmp byte [StrBuffer + 1], 0
        je .InputLeftBorder

        stdcall PrintString, Newline
        stdcall PrintString, InvalidFunction
        jmp .InputFunctionsLoop

        .InputNextFunction:
            add bx, ax
            add bx, 2
            inc [FunctionsCount]
            cmp [FunctionsCount], MAX_FUNCTIONS_COUNT
            je .InputLeftBorder

            stdcall PrintString, Newline
            stdcall PrintString, InputOneMoreFunction

            jmp .InputFunctionsLoop

    .InputParametric:
    xor cx, cx
    mov bx, FunctionsRPNs
    .InputParametricLoop:
       cmp cx, 2
       je .InputTMin

        mov dx, InputXt
        cmp cx, 0
        je @F
        mov dx, InputYt

        @@:
        stdcall PrintString, dx

        stdcall InputString, StrBuffer
        add dx, 2
        push cx
        stdcall ToRPN, dx, bx, 't'
        pop cx
        stdcall PrintString, Newline
        jnc @F

        mov ax, [bx]
        add bx, ax
        add bx, 2
        inc cx
        jmp .InputParametricLoop

        @@:
        stdcall PrintString, InvalidParametricExpression
        stdcall PrintString, Newline

        jmp .InputParametricLoop

     .InputTMin:
     stdcall PrintString, InputTMin
     .InputTMinLoop:
        stdcall InputConstantExpression, StrBuffer, SegmentBorderRPN, TFrom
        jc .InputTMax

        stdcall PrintString, Newline
        stdcall PrintString, InvalidExpression
        jmp .InputTMinLoop


     .InputTMax:
     stdcall PrintString, Newline
     stdcall PrintString, InputTMax
     .InputTMaxLoop:
        stdcall InputConstantExpression, StrBuffer, SegmentBorderRPN, TTo
        jc .InputLeftBorder

        stdcall PrintString, Newline
        stdcall PrintString, InvalidExpression
        jmp .InputTMaxLoop


    .InputLeftBorder:
    stdcall PrintString, Newline
    stdcall PrintString, InputLeftBorder
    .InputLeftBorderLoop:
        stdcall InputConstantExpression, StrBuffer, SegmentBorderRPN, XFrom
        jc .InputRightBorder

        cmp byte [StrBuffer + 1], 0
        je @F

        stdcall PrintString, Newline
        stdcall PrintString, InvalidExpression
        jmp .InputLeftBorderLoop

    .InputRightBorder:
    stdcall PrintString, Newline
    stdcall PrintString, InputRightBorder
    .InputRightBorderLoop:
        stdcall InputConstantExpression, StrBuffer, SegmentBorderRPN, XTo
        jc .CheckBorders

        stdcall PrintString, Newline
        stdcall PrintString, InvalidExpression
        jmp .InputRightBorderLoop

        .CheckBorders:
        fld [XFrom]
        fld [XTo]
        fcomip st0, st1
        fstp st0
        ja @F

        stdcall PrintString, Newline
        stdcall PrintString, InvalidBorders
        jmp .InputRightBorderLoop

    @@:
    mov ax, cs
    add ax, 1000h
    mov es, ax

    fstcw [FPUControlWord]
    or [FPUControlWord], ROUND_ZERO_MASK
    fldcw [FPUControlWord]

    stdcall InitValues
    stdcall SetVideoMode, 13h
    mov [OldVideoModeInfo], ax
    stdcall DrawAll
    stdcall HandleKeys

    stdcall SetVideoMode, [OldVideoModeInfo]
    ret


proc HandleKeys
    .HandleLoop:
        call ReadKey
        cmp ah, KEY_RIGHT
        jne @F
        mov al, DIR_RIGHT
        jmp .Shift

        @@:
        cmp ah, KEY_LEFT
        jne @F
        mov al, DIR_LEFT
        jmp .Shift

        @@:
        cmp ah, KEY_UP
        jne @F
        mov al, DIR_UP
        jmp .Shift

        @@:
        cmp ah, KEY_DOWN
        jne @F
        mov al, DIR_DOWN
        jmp .Shift

        @@:
        cmp ah, KEY_CTRL_PLUS
        jne @F
        xor al, al
        jmp .Scale

        @@:
        cmp ah, KEY_CTRL_MINUS
        jne @F
        mov al, 1
        jmp .Scale

        @@:
        cmp al, KEY_ESC
        je .Return

        @@:
        cmp ah, KEY_ALT_X
        jne .HandleLoop
        jmp .Return

        .Shift:
        stdcall ShiftImage
        jmp .HandleLoop

        .Scale:
        stdcall ScaleImage

        jmp .HandleLoop

    .Return:
    ret
endp


; AL - direction (0 - right, 1 - left, 2 - up, 3 - down)
proc ShiftImage uses bx
    local pCoef dw ?
    local pCoordFrom dw ?
    local pCoordTo dw ?

    test al, DIR_RIGHT or DIR_LEFT
    jz .YCoef
    mov [pCoef], XScaleCoef
    mov [pCoordFrom], XFrom
    mov [pCoordTo], XTo
    jmp @F

    .YCoef:
    mov [pCoef], YScaleCoef
    mov [pCoordFrom], YFrom
    mov [pCoordTo], YTo

    @@:
    mov bx, [pCoef]
    fild [ShiftPixels]
    fld qword [bx]
    fdivp
    test al, DIR_LEFT or DIR_DOWN
    jz @F
    fchs

    @@:
    mov bx, [pCoordFrom]
    fld qword [bx]
    fadd st0, st1
    fstp qword [bx]

    mov bx, [pCoordTo]
    fld qword [bx]
    fadd st0, st1
    fstp qword [bx]

    fstp st0
    call Redraw
    ret
endp


; AL - 0 - zoom in, 1 - zoom out
proc ScaleImage
     fld [ScaleCoef]
     test al, al
     jz @F
     fld1
     fdivrp

     @@:
     fld [XScaleCoef]
     fmul st0, st1
     fstp [XScaleCoef]

     fld [YScaleCoef]
     fmul st0, st1
     fstp [YScaleCoef]

     fld [XFrom]
     fdiv st0, st1
     fstp [XFrom]

     fld [XTo]
     fdiv st0, st1
     fstp [XTo]

     fld [YFrom]
     fdiv st0, st1
     fstp [YFrom]

     fld [YTo]
     fdiv st0, st1
     fstp [YTo]

     cmp [PlotType], REGULAR_PLOT
     jne @F

     fld [XTo]
     fsub [XFrom]
     fidiv [PointsCount]
     fstp [Step]

     @@:
     fstp st0

     call Redraw
     ret
endp


proc InitValues  uses si di
    mov si, XFrom
    mov di, XTo

    cmp [PlotType], PARAMETRIC_PLOT
    jne @F

    mov si, TFrom
    mov di, TTo

    @@:
    fld qword [di]
    fsub qword [si]
    fidiv [PointsCount]
    fstp [Step]

    fild [ScreenWidth]
    fld [XTo]
    fsub [XFrom]
    fdivp
    fst [XScaleCoef]
    fstp [YScaleCoef]

    fld [XTo]
    fsub [XFrom]
    fimul [ScreenHeight]
    fidiv [ScreenWidth]
    fdiv [Two]
    fst [YTo]
    fchs
    fstp [YFrom]

    ret
endp


proc ClearScreen
    xor di, di
    xor ax, ax
    mov cx, SCREEN_HEIGHT * SCREEN_WIDTH / 2
    rep stosw
    ret
endp


proc DrawAll uses bx
    stdcall DrawAxes

    cmp [PlotType], PARAMETRIC_PLOT
    jne @F

    fld [TFrom]
    fstp [CurT]

    mov ax, FunctionsRPNs
    add ax, word [FunctionsRPNs]
    add ax, 2
    stdcall DrawPlot, PARAMETRIC_PLOT, FIRST_PLOT_COLOR, FunctionsRPNs, ax
    jmp .Return

    @@:
    mov cx, [FunctionsCount]
    mov bx, FunctionsRPNs
    mov dx, FIRST_PLOT_COLOR
    fld [XFrom]
    fstp [CurX]
    .DrawPlotsLoop:
        push cx dx
        stdcall DrawPlot, REGULAR_PLOT, dx, bx, 0
        pop dx cx
        add bx, [bx]
        add bx, 2
        add dx, 10h
        loop .DrawPlotsLoop

    .Return:
    ret
endp


proc Redraw
   stdcall ClearScreen
   stdcall DrawAll
   ret
endp


proc DrawAxes
    mov al, AXES_COLOR

    fld [YTo]
    fmul [YScaleCoef]
    fst [YOffset]

    fldz
    fcomip st, st1
    jb @F
    fstp st0
    fldz
    jmp .DrawX

    @@:
    fild [ScreenHeightMinus1]
    fcomip st, st1
    ja .DrawX
    fstp st0
    fild [ScreenHeightMinus1]

    .DrawX:
    fistp [XAxisOffset]
    mov di, [XAxisOffset]
    imul di, SCREEN_WIDTH
    mov cx, SCREEN_WIDTH
    rep stosb

    fld [XFrom]
    fmul [XScaleCoef]
    fchs
    fst [XOffset]

    fldz
    fcomip st, st1
    jb @F
    fstp st0
    fldz
    jmp .DrawY

    @@:
    fild [ScreenWidthMinus1]
    fcomip st, st1
    ja .DrawY
    fstp st0
    fild [ScreenWidthMinus1]

    .DrawY:
    fistp [YAxisOffset]
    mov di, [YAxisOffset]
    mov cx, SCREEN_HEIGHT
    .DrawYLoop:
    mov [es:di], al
    add di, SCREEN_WIDTH
    loop .DrawYLoop

    ret
endp

; AX - Number
Abs:
    cwd
    xor ax, dx
    sub ax, dx
    ret

; AX - Number
; Returns: DX - 1 if positive, -1 if negative
Sign:
    cwd
    or dx, 1
    ret


proc DrawLine uses bx si di, X1:WORD, Y1:WORD, X2:WORD, Y2:WORD, Color:BYTE
     local SignX dw ?
     local SignY dw ?
     local Error dw ?

     mov ax, [Y2]
     sub ax, [Y1]
     call Sign
     mov [SignY], dx
     call Abs
     mov cx, ax
     neg cx

     mov ax, [X2]
     sub ax, [X1]
     call Sign
     mov [SignX], dx
     call Abs
     mov dx, ax

     mov si, [X1]
     mov bx, [Y1]

     mov [Error], dx
     add [Error], cx

     .DrawLoop:
         mov di, bx
         imul di, SCREEN_WIDTH
         add di, si
         mov al, [Color]
         mov [es:di], al

         cmp si, [X2]
         jne @F
         cmp bx, [Y2]
         jne @F
         jmp .Return

         @@:
         mov ax, [Error]
         shl ax, 1

         cmp ax, cx
         jl @F

         cmp si, [X2]
         je .Return
         add [Error], cx
         add si, [SignX]

         @@:
         cmp ax, dx
         jg .DrawLoop

         cmp bx, [Y2]
         je .Return
         add [Error], dx
         add bx, [SignY]

         jmp .DrawLoop

     .Return:
     ret
endp


proc CalcRegularPlotPoint pRPN:WORD
    fclex
    stdcall CalcRPN, [pRPN], CurY, CurX

    fld [CurX]
    fadd [Step]
    fstp [CurX]

    ret
endp


proc CalcParametricPlotPoint pRPNXt:WORD, pRPNYt:WORD
    fclex
    stdcall CalcRPN, [pRPNXt], CurX, CurT
    stdcall CalcRPN, [pRPNYt], CurY, CurT

    fld [CurT]
    fadd [Step]
    fstp [CurT]

    ret
endp


proc DrawPlot uses bx si di, PlotType:WORD, Color, pRPN1:WORD, pRPN2:WORD
    local PrevXScreen dw ?
    local PrevYScreen dw ?
    local CurFloatYScreen dq ?
    local PrevFloatYScreen dq ?

    mov bx, FIRST_POINT
    xor dx, dx

    mov cx, [PointsCount]
    fld [XFrom]
    fstp [CurX]
    .PlotLoop:
        xor ax, ax
        cmp [PlotType], PARAMETRIC_PLOT
        jne @F

        inc ax
        push ax dx cx
        stdcall CalcParametricPlotPoint, [pRPN1], [pRPN2]
        pop cx dx ax

        ;fld [CurdX]
        ;fld [XFrom]
        ;fcomip st0, st1
        ;ja .SkipBelow
        ;fld [XTo]
        ;fcomip st0, st1
        ;jb .SkipAbove
        ;fstp st0

        @@:
        fld [CurX]
        fmul [XScaleCoef]
        fadd [XOffset]
        fild [ScreenWidthMinus1]
        fcomip st0, st1
        jb .SkipXOutOfBounds
        fldz
        fcomip st0, st1
        ja .SkipXOutOfBounds
        fistp [XScreen]
        test ax, ax
        jnz @F

        push dx cx
        stdcall CalcRegularPlotPoint, [pRPN1]
        pop cx dx

        @@:
        fstsw ax
        test ax, 1
        jnz .SkipFunctionUndefined

        fld [CurY]
        fmul [YScaleCoef]
        fchs
        fadd [YOffset]
        fst [CurFloatYScreen]
        fild [ScreenHeightMinus1]
        fcomip st0, st1
        jb .SkipAbove
        fldz
        fcomip st0, st1
        ja .SkipBelow
        fistp [YScreen]

        cmp bx, FIRST_POINT
        je .ValueOk
        cmp bx, VALUE_UNDEFINED
        je .ValueOk
        cmp bx, X_OUT_OF_BOUNDS
        je .ValueOk
        cmp bx, VALUE_OK
        je .Draw

        mov bx, VALUE_OK
        lea si, [PrevXScreen]
        lea di, [PrevYScreen]

        .DrawOutOfBounds:
        fldz
        cmp dx, 0
        jl @F
        fiadd [ScreenHeight]

        @@:
        fld st0
        fsub [PrevFloatYScreen]
        fild [XScreen]
        fisub [PrevXScreen]
        fmulp
        fld [CurFloatYScreen]
        fsub [PrevFloatYScreen]
        fdivp
        fiadd [PrevXScreen]
        fistp word [si]
        fistp word [di]
        jmp .Draw

        .PlotLoop2: ; костыль for problem "relative jump out of range"
        jmp .PlotLoop

        .Draw:
        push cx
        push dx
        stdcall DrawLine, [PrevXScreen], [PrevYScreen], [XScreen], [YScreen], [Color]
        pop dx
        pop cx
        jmp .NextIteration

        .SkipAbove:
        mov dx, 1
        jmp .SkipYOutOfBounds

        .SkipBelow:
        mov dx, -1

        .SkipYOutOfBounds:
        fstp st0
        cmp bx, VALUE_OK
        mov bx, Y_OUT_OF_BOUNDS
        jne .NextIteration
        mov si, XScreen
        mov di, YScreen
        jmp .DrawOutOfBounds

        .SkipFunctionUndefined:
        mov bx, VALUE_UNDEFINED
        jmp .NextIteration

        .SkipXOutOfBounds:
        mov bx, X_OUT_OF_BOUNDS
        fstp st0
        jmp .NextIteration

        .ValueOk:
        mov bx, VALUE_OK
        .NextIteration:
        mov ax, [YScreen]
        mov [PrevYScreen], ax
        fld [CurFloatYScreen]
        fst [PrevFloatYScreen]
        fistp [PrevYScreen]
        mov ax, [XScreen]
        mov [PrevXScreen], ax
        loop .PlotLoop2

    push es ds
    mov ax, es
    mov ds, ax
    mov ax, $A000
    mov es, ax

    mov cx, 32000
    xor si, si
    xor di, di
    rep movsw

    pop ds es

    ret
endp


proc SetVideoMode ModeInfo:Word
    mov ah, 0Fh
    int 10h
    mov bl, al

    movzx ax, byte [ModeInfo]
    int 10h

    mov ah, 05h
    mov al, byte [ModeInfo + 1]
    int 10h

    xchg ax, bx
    ret
endp


; Returns
; AX - 00cc if regular ascii character
;    - cc00 if extended ascii character
proc ReadKey
   xor dx, dx
   mov ax, $0C08
   int 21h
   add  dl, al
   jnz @F
   mov ah, $08
   int  21h
   mov dh, al

   @@:
   xchg ax, dx
   ret
endp


Newline db 13, 10, '$'
ChoosePlotType db "Choose plot type:", 13, 10, "1) Regular", 13, 10, "2) Parametric", 13, 10, '$'
InputXt db "Input x(t) = $"
InputYt db "Input y(t) = $"
InvalidParametricExpression db "Invalid parametric expression, try again: $"
InvalidPlotType db "Invalid plot type, try again: $"
InputTMin db "Input t_min: $"
InputTMax db "Input t_max: $"
InputFunction db "Input function: y = $"
InputOneMoreFunction db "Input one more function (press enter to skip): y = $"
InputLeftBorder db "Input left border (press Enter to use default [-5; 5]): $"
InputRightBorder db "Input right border: $"
InvalidFunction db "Invalid function, try again: y = $"
InvalidExpression db "Invalid expression, try again: $"
InvalidBorders db "Right border can't be lower than left, try again: $"

FunctionsCount dw 0
PointsCount dw 1000
ShiftPixels dw 10
ScaleCoef dq 1.5
Two dq 2.0
ScreenWidth dw SCREEN_WIDTH
ScreenHeight dw SCREEN_HEIGHT
ScreenHeightMinus1 dw SCREEN_HEIGHT - 1
ScreenWidthMinus1 dw SCREEN_WIDTH - 1
HalfScreenHeight dw SCREEN_HEIGHT / 2

XFrom dq -5.0
XTo dq 5.0

TFrom dq -6.0
TTo dq 6.0

PlotType db REGULAR_PLOT

FPUControlWord dw ?

YFrom dq ?
YTo dq ?
XOffset dq ?
YOffset dq ?
XAxisOffset dw ?
YAxisOffset dw ?
YScreen dw ?
XScreen dw ?
CurX dq ?
CurY dq ?
CurT dq ?
Step dq ?
XScaleCoef dq ?
YScaleCoef dq ?

OldVideoModeInfo dw ?

StrBuffer db 255, 0, 254 dup(?)
SegmentBorderRPN db 256 dup(?)
FunctionsRPNs db 256 dup(?)