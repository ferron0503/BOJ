program main
 integer :: n, k, r, c, supr, infr, supc, infc
integer :: supcount, infcount
logical :: x, y

read*, n, r, c
k = 2**n

supr = k
supc = k
infr = 1
infc = 1
infcount = 0
supcount = k**2

do while((supr - infr) > 1 .or. (supc - infc) > 1)
    midr = (infr + supr) / 2
    midc = (infc + supc) / 2
    if (r < midr .and. c < midc) then
      supr = midr
      supc = midc
      supcount = supcount / 4

    else if (r < midr .and. c >= midc) then
      infr = midr
      supc = midc
      infcount = infcount + supcount / 4
      supcount = supcount / 4

    else if (r >= midr .and. c < midc) then
      supr = midr
      infc = midc
      infcount = infcount + supcount / 2
      supcount = supcount / 4

    else if (r >= midr .and. c >= midc) then
      infr = midr
      infc = midc
      infcount = infcount + 3 * supcount / 4
      supcount = supcount / 4
    end if
end do
  
if (r == infr .and. c == infc) then
    print '(i0)', infcount
else if (r == infr .and. c == supc) then
    print '(i0)', infcount + 1
else if (r == supr .and. c == infc) then
    print '(i0)', infcount + 2
else if (r == supr .and. c == supc) then
    print '(i0)', infcount + 3
end if

end program
