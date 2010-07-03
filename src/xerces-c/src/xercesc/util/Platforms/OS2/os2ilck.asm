        title os2ilck.asm
	.486

CODE32	segment dword use32 public 'CODE'
CODE32	ends
DATA32	segment dword use32 public 'DATA'
DATA32	ends
DGROUP	group  DATA32
	assume	cs:FLAT, ds:FLAT, ss:FLAT, es:FLAT
CODE32	segment
	align 04h
os2InterlockedCompareExchange    proc 
       mov ecx, [esp+4]
       mov edx, [esp+8]
       mov eax, [esp+12]
       lock cmpxchg [ecx], edx
       ret
os2InterlockedCompareExchange    endp
	align 04h
os2InterlockedIncrement  proc  
        mov     eax,[esp+4]
        lock    inc dword ptr[eax]
        ret	
os2InterlockedIncrement  endp
	align 04h
os2InterlockedDecrement  proc  
        mov     eax,[esp+4]
        lock    dec dword ptr[eax]
        ret	
os2InterlockedDecrement  endp

public os2InterlockedCompareExchange
public os2InterlockedIncrement
public os2InterlockedDecrement
CODE32	ends
end

