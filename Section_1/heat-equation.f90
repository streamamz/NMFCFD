!熱伝導方程式
!初期条件に周期境界条件を取る
program main
    implicit none
        real(8), parameter :: pi = atan(1.0d0) * 4.0d0
        real(8) :: dx
        real(8), allocatable :: u(:), tmp(:)
        integer :: n, step, i, j

        n = 100
        step = 1000

        dx = 0.01d0

        allocate(u(n), tmp(n))
    
        do i = 1, n
            u(i) = sin(2.0d0*pi*dx*real(i-1,8))
        end do
        call show(u,n,dx)

        do j = 1, step
            tmp = u
            do i = 1, n
                if (i == 1) then
                    u(1) = tmp(1) + 0.5d0*(tmp(2)-2.0d0*tmp(1)+tmp(n))
                else if (i == n) then
                    u(n) = tmp(n) + 0.5d0*(tmp(1)-2.0d0*tmp(n)+tmp(n-1))
                else
                    u(i) = tmp(i) + 0.5d0*(tmp(i+1)-2.0d0*tmp(i)+tmp(i-1))
                end if
            end do
            call show(u,n,dx)
        end do

        deallocate(u,tmp)
    stop
    contains
    subroutine show(u,n,dx)
        implicit none
            real(8), intent(in) :: u(n), dx
            integer :: n, i
            do i = 1, n
                write(*,*) dx*real(i-1,8), u(i)
            end do
            write(*,*) dx*real(n,8),u(1)
            write(*,*)
        return
    end subroutine show
end program main