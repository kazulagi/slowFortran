module slowdp
    use iso_fortran_env
    implicit none

    type :: realpointer_
        real(real64),pointer :: val
    end type

    type :: realpointerarray_
        type(realpointer_),allocatable :: array(:)
    end type
end module 


program main
    use slowdp
    implicit none

    real(real64),target :: t0, t1, val,dp
    integer(int32),parameter :: n=1000000
    integer(int32) :: i
    real(real64),target :: vector(n)
    type(realpointerarray_) :: ra

    print *, "Yukkuri shite ittene!"


    ! set value
    val = 1.0d0
    vector(:) = val
    allocate(ra%array(n) )
    do i=1,n
        ra%array(i)%val => vector(i)
    enddo

    call cpu_time(t0)
    dp=0.0d0
    do i=1,n
        dp = dp + ra%array(i)%val*ra%array(i)%val
    enddo
    call cpu_time(t1)

    print *, "Yukkuri-time :: ",abs(t1-t0),"sec."
    
    call cpu_time(t0)
    dp=dot_product(vector,vector)
    call cpu_time(t1)
    print *, "dot_product time :: ",abs(t1-t0),"sec."
end program main
