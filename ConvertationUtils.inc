


proc DoubleToString stdcall uses di bx, numBuffer:WORD, strBuffer:WORD, precision:WORD
    local numCopy dq ?
    local wholePart dd ?
    local multiplier dd 10.0

    mov bx, [numBuffer]
    fld qword [bx]
    fstp [numCopy]

    mov di, [strBuffer]

    fld [numCopy]
    fldz
    fcomip st1
    jb @F
    mov [di], byte '-'
    inc di
    fchs

    @@:
    fistp [wholePart]
    stdcall Int32ToString, word [wholePart], word [wholePart + 2], di
    mov di, ax

    mov [di], byte '.'
    inc di
    fld [numCopy]
    mov cx, [precision]
    .ConvertFractionalPartLoop:
        fisub [wholePart]
        fmul [multiplier]
        fld st0
        fistp [wholePart]
        mov ax, word [wholePart]
        add ax, '0'
        mov [di], al
        inc di
        loop .ConvertFractionalPartLoop

    mov [di], byte 0
    ret
endp


proc StringToDouble uses si di, strBuffer:WORD, strLen:BYTE, numBuffer:WORD
     local multiplier1 dd 10.0
     local multiplier2 dd 0.1
     local currentDigit dw ?

     mov si, [strBuffer]
     mov di, [numBuffer]
     movzx cx, [strLen]

     fldz

     xor dx, dx
     cmp [si], byte '-'
     jne .WholePartLoop

     inc si
     dec cx
     inc dx
     .WholePartLoop:
         movzx ax, byte [si]
         inc si
         cmp ax, '.'
         jne @F

         dec cx
         jmp .ConvertFractionalPart

         @@:
         stdcall IsDigit, ax
         jnc .ReturnError

         fmul [multiplier1]
         sub ax, '0'
         mov [currentDigit], ax
         fiadd [currentDigit]
         loop .WholePartLoop

     .ConvertFractionalPart:
     cmp cx, 0
     je .ReturnSuccess
     .FractionalPartLoop:
         movzx ax, byte [si]
         inc si

         stdcall IsDigit, ax
         jnc .ReturnError

         sub ax, '0'
         mov [currentDigit], ax
         fild [currentDigit]
         fmul [multiplier2]
         faddp

         fld [multiplier2]
         fdiv [multiplier1]
         fstp [multiplier2]
         loop .FractionalPartLoop

     .ReturnSuccess:
         test dx, dx
         jz @F
         fchs

         @@:
         fstp qword [di]
         stc
         jmp .Return

     .ReturnError:
         fstp st0
         clc
         jmp .Return

      .Return:
          ret
endp


proc Int32Div stdcall uses bx di, num:DWORD, divider:WORD, resultBuf:WORD
    mov bx, [divider]
    mov di, [resultBuf]

    xor dx, dx
    mov ax, word [di + 2]
    div bx

    mov word [di + 2], ax
    mov ax, word [num]
    div bx

    mov word [di], ax
    mov ax, dx
    ret
endp


proc Int32ToString stdcall uses di, num:DWORD, strBuffer:WORD
     mov di, [strBuffer]

     xor cx, cx
     .GetDigitsLoop:
         lea ax, [num]
         stdcall Int32Div, word [num], word [num + 2], 10, ax
         add ax, '0'
         push ax
         inc cx

         cmp [num], 0
         ja .GetDigitsLoop

     .WriteNumLoop:
         pop ax
         mov [di], al
         inc di
         loop .WriteNumLoop

     mov ax, di
     ret
endp
