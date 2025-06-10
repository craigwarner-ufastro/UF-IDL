function ishft2, array, nbit_shift

return, ishft( ishft( array, nbit_shift ), -nbit_shift )
end
