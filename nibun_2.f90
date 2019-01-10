program nibun

    implicit none
    
    double precision:: a,b,c
    double precision,parameter :: e =0.001_8
    integer(4)::i
    double precision , parameter :: zero = 0.0_8
    
    !read(*,*) a,b
    
    a=pi_func(a);
    b=pi_func(b);   

        do i=1,10
            c=(a+b)/2
            c=pi_func(c)
            if(dabs(pi_func(c)) <= e) then
                exit
            else if( (pi_func(a)*pi_func(c)) > zero)then
                a=c 
            else if( (pi_func(b)*pi_func(c)) > zero ) then
                b=c
            end if
       end do
    
    write(*,*) c
    write(*,*) '厳密解は 2'
    contains
    double precision function pi_func(x)
        implicit none
        double precision:: x
        pi_func= (x-2.0)*(x+1.0)
        end function pi_func 
    end program nibun                   