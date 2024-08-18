program main
implicit none
integer :: n, i, j, rslt
integer, dimension(:), allocatable :: windowSTAT

read*, n
allocate(windowSTAT(n))
windowSTAT = 0


do i=1,n
    
    j = 1
    do while(i*j <= n)
    if (windowSTAT(i*j) == 0) then
      windowSTAT(i*j) = 1
    else
      windowSTAT(i*j) = 0
    end if
    j = j + 1
    end do
    
end do

rslt = count(windowSTAT == 1)

print '(i0)', rslt

end program
