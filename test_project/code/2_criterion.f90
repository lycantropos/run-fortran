module criterion
    use, intrinsic :: iso_fortran_env, dp=>real64
    use derived_types, only: Star
    implicit none

    private

    public :: splitDataForUVWvsUVW, &
              splitDataForUVWvsMbol

contains

    subroutine splitDataForUVWvsUVW(whiteDwarfs, &
                                    sampleUvsV, &
                                    sampleUvsW, &
                                    sampleVvsW)
        type (Star), dimension(:), intent(in) :: whiteDwarfs
        type (Star), dimension(:), allocatable, intent(out) :: sampleUvsV
        type (Star), dimension(:), allocatable, intent(out) :: sampleUvsW
        type (Star), dimension(:), allocatable, intent(out) :: sampleVvsW
        real(dp) :: highestCoordValue
        integer :: xHighestCounter = 0, &
                   yHighestCounter = 0, &
                   zHighestCounter = 0, &
                   counterUvsV = 0, &
                   counterUvsW = 0, &
                   counterVvsW = 0
        integer :: i

        ! Calculating number of WDs for each plot
        do i = 1, size(whiteDwarfs)
            highestCoordValue = maxval(abs(whiteDwarfs(i)%coords))
            if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(1))) &
                .lt. 1.0d-5) then
                xHighestCounter = xHighestCounter + 1
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(2))) &
                .lt. 1.0d-5) then
                yHighestCounter = yHighestCounter + 1
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(3))) &
                .lt. 1.0d-5) then
                zHighestCounter = zHighestCounter + 1
            end if
        end do
        
        allocate(sampleUvsV(zHighestCounter))
        allocate(sampleUvsW(yHighestCounter))
        allocate(sampleVvsW(xHighestCounter))

        ! Distributing WDs in samples for each plot
        do i = 1, size(whiteDwarfs)
            highestCoordValue = maxval(abs(whiteDwarfs(i)%coords))
            if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(1))) &
                .lt. 1.0d-5) then
                counterVvsW = counterVvsW + 1
                sampleVvsW(counterVvsW) = whiteDwarfs(i)
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(2))) &
                .lt. 1.0d-5) then
                counterUvsW = counterUvsW + 1
                sampleUvsW(counterUvsW) = whiteDwarfs(i)
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(3))) &
                .lt. 1.0d-5) then
                counterUvsV = counterUvsV + 1
                sampleUvsV(counterUvsV) = whiteDwarfs(i)
            end if
        end do
    end subroutine splitDataForUVWvsUVW


    subroutine splitDataForUVWvsMbol(whiteDwarfs, &
                                     sampleUvsMbol, &
                                     sampleVvsMbol, &
                                     sampleWvsMbol)
        type (Star), dimension(:), intent(in) :: whiteDwarfs
        type (Star), dimension(:), allocatable, intent(out) :: sampleUvsMbol
        type (Star), dimension(:), allocatable, intent(out) :: sampleVvsMbol
        type (Star), dimension(:), allocatable, intent(out) :: sampleWvsMbol
        real(dp) :: highestCoordValue
        integer :: xHighestCounter = 0, &
                   yHighestCounter = 0, &
                   zHighestCounter = 0, &
                   counterUvsMbol = 0, &
                   counterVvsMbol = 0, &
                   counterWvsMbol = 0
        integer :: i

        ! Calculating number of WDs for each plot
        do i = 1, size(whiteDwarfs)
            highestCoordValue = maxval(abs(whiteDwarfs(i)%coords))
            if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(1))) &
                .lt. 1.0d-5) then
                xHighestCounter = xHighestCounter + 1
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(2))) &
                .lt. 1.0d-5) then
                yHighestCounter = yHighestCounter + 1
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(3))) &
                .lt. 1.0d-5) then
                zHighestCounter = zHighestCounter + 1
            end if
        end do
        
        allocate(sampleUvsMbol(yHighestCounter + zHighestCounter))
        allocate(sampleVvsMbol(xHighestCounter + zHighestCounter))
        allocate(sampleWvsMbol(xHighestCounter + yHighestCounter))

        ! Distributing WDs in samples for each plot
        do i = 1, size(whiteDwarfs)
            highestCoordValue = maxval(abs(whiteDwarfs(i)%coords))
            if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(1))) &
                .lt. 1.0d-5) then
                counterVvsMbol = counterVvsMbol + 1
                counterWvsMbol = counterWvsMbol + 1
                sampleVvsMbol(counterVvsMbol) = whiteDwarfs(i)
                sampleWvsMbol(counterWvsMbol) = whiteDwarfs(i)
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(2))) &
                .lt. 1.0d-5) then
                counterUvsMbol = counterUvsMbol + 1
                counterWvsMbol = counterWvsMbol + 1
                sampleUvsMbol(counterUvsMbol) = whiteDwarfs(i)
                sampleWvsMbol(counterWvsMbol) = whiteDwarfs(i)
            else if (abs(highestCoordValue - abs(whiteDwarfs(i)%coords(3))) &
                .lt. 1.0d-5) then
                counterUvsMbol = counterUvsMbol + 1
                counterVvsMbol = counterVvsMbol + 1
                sampleUvsMbol(counterUvsMbol) = whiteDwarfs(i)
                sampleVvsMbol(counterVvsMbol) = whiteDwarfs(i)
            end if
        end do
    end subroutine splitDataForUVWvsMbol

    
end module criterion