module files

implicit none

private
public getNumberOfLines, getNewUnit


contains
    
    ! Returns number of lines in file
    function getNumberOfLines(filePath) result(n)
        character(len = *), intent(in) :: filePath
        integer :: n, ioStatus, u
        n = 0
        open(getNewUnit(u), file = filePath)
        do
            read(u, *, iostat = ioStatus)
            if(is_iostat_end(ioStatus)) exit
            n = n + 1
        end do
        close(u)
    end function

    ! Returns lowest i/o unit number not in use
    function getNewUnit(unit) result(n)
        integer, intent(out), optional :: unit
        integer, parameter :: nmin=10   
        integer, parameter :: nmax=999  
        integer n
        logical inuse
        do n = nmin, nmax
            inquire(unit=n, opened=inuse)
            if (.not. inuse) then
                if (present(unit)) unit = n
                return
            end if
        end do
        print*, "getNewUnit ERROR: available unit not found."
    end function

end module