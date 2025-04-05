


proc IsDigit char:WORD
    mov ax, [char]

   cmp ax, '0'
   jb .NotDigit
   cmp ax, '9'
   ja .NotDigit

   stc
   ret

   .NotDigit:
       clc
       ret
endp


proc IsLetter char:WORD
    mov ax, [char]
    or ax, 32
    cmp ax, 'a'
    jb .NotLetter
    cmp ax, 'z'
    ja .NotLetter

    stc
    ret

    .NotLetter:
        clc
        ret
endp


proc IsAlphanumeric char:WORD
    stdcall IsLetter, [char]
    jc .Alphanumeric
    stdcall IsDigit, [char]
    jc .Alphanumeric

    clc
    ret

    .Alphanumeric:
        stc
        ret
endp


proc StringsEqual uses si di, str1Buf:WORD, str1Len:WORD, str2Buf:WORD, str2Len:WORD
   mov ax, [str1Len]
   cmp ax, [str2Len]
   jne .NotEqual

   mov si, [str1Buf]
   mov di, [str2Buf]
   mov cx, [str1Len]
   inc cx
   repe cmpsb
   cmp cx, 0
   jne .NotEqual
   .Equal:
      stc
      jmp .Return

   .NotEqual:
      clc
      jmp .Return

   .Return:
       ret
endp