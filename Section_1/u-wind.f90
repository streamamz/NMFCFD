!風上差分
!前進差分と後退差分を使った際の違いを確認する
program main
    implicit none
        real(8) :: c, dx, dt
        real(8), allocatable :: u(:)
        integer :: n, step, i

        n = 101
        step = 50

        dx = 0.01d0
        dt = 0.01d0
        c = 1.0d0

        allocate(u(n))

        do i = 1, n
            if (i<=51) then
                u(i) = 1.0d0
            else
                u(i) = 0.0d0
            end if
        end do

        call show(u,n,dx)

        !良い風上差分
        !call good(u,dx,dt,c,n,step)
        !悪い風上差分
        !call bad(u,dx,dt,c,n,step)

        deallocate(u)
    stop
    contains
    subroutine show(u,n,dx)
        implicit none
            real(8), intent(in) :: u(n), dx
            integer :: n, i
            do i = 1, n
                write(*,*) dx*real(i-1,8), u(i)
            end do
            write(*,*)
        return
    end subroutine show
    !良い風上差分
    subroutine good(u,dx,dt,c,n,step)
        implicit none
            integer :: n, step
            real(8) :: u(n), dx, dt, c
            integer :: i, j
            real(8), allocatable :: tmp(:)
            allocate(tmp(n))
            do j = 1, step
                tmp = u
                do i = 1,n
                    if (.not.i==1) then
                        u(i) = tmp(i) - c*dx/dt*(tmp(i)-tmp(i-1))
                    end if
                end do
                call show(u,n,dx)
            end do
            deallocate(tmp)
        return
    end subroutine good
    !悪い風上差分
    subroutine bad(u,dx,dt,c,n,step)
        implicit none
            integer :: n, step
            real(8) :: u(n), dx, dt, c
            integer :: i, j
            real(8), allocatable :: tmp(:)
            allocate(tmp(n))
            do j = 1, step
                tmp = u
                do i = 1,n
                    if (.not.i==n) then
                        u(i) = tmp(i) - c*dx/dt*(tmp(i+1)-tmp(i))
                    end if
                end do
                call show(u,n,dx)
            end do
            deallocate(tmp)
        return
        end subroutine bad
end program main