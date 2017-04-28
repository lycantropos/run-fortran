module plot_mbol


    use commons, only: OUTPUT_FORMAT

    implicit none

    public :: fillSelectedDataForMbolCloud, &
              fillSelectedDataForMbolBins, &
              fillFullDataForMbolBins, &
              writeSelectedDataForMbolBins

    private :: MBOL_MIN, &
               MBOL_MAX, &
               MBOL_INC, &
               NUM_OF_BINS
        real*8, parameter :: MBOL_MIN = 5.75d0                                      
        real*8, parameter :: MBOL_MAX = 20.75d0
        real*8, parameter :: MBOL_INC = 0.5d0
        integer, parameter :: NUM_OF_BINS = int((MBOL_MAX - MBOL_MIN) &
                                                / MBOL_INC)

contains


    subroutine fillSelectedDataForMbolCloud(highest, &
                                            magnitude, &
                                            vel_hel, &
                                            unitCloudU, &
                                            unitCloudV, &
                                            unitCloudW)
        
        character(len = 1), intent(in) :: highest
        real*8, intent(in) :: magnitude
        real*8, dimension(3), intent(in) :: vel_hel
        integer, intent(in) :: unitCloudU, &
                               unitCloudV, &
                               unitCloudW
        character(len = *), parameter :: CLOUD_FORMAT = '(2(f12.6,3x))'

        
        select case (highest)
            case("x")
                write(unitCloudV, CLOUD_FORMAT) magnitude, vel_hel(2)
                write(unitCloudW, CLOUD_FORMAT) magnitude, vel_hel(3)
            case("y")
                write(unitCloudU, CLOUD_FORMAT) magnitude, vel_hel(1)
                write(unitCloudW, CLOUD_FORMAT) magnitude, vel_hel(3)
            case("z")
                write(unitCloudU, CLOUD_FORMAT) magnitude, vel_hel(1)
                write(unitCloudV, CLOUD_FORMAT) magnitude, vel_hel(2)
            case default
                print*, "Error: couldn't determine highest coordinate"
        end select
        
    end subroutine fillSelectedDataForMbolCloud


    subroutine fillSelectedDataForMbolBins(highest, &
                                           vel_hel, &
                                           magnitude, &
                                           numberOfWDsInBin, &
                                           sumOfVelocitiesInBin, &
                                           velocityArrayForMbol)
        
        character(len = 1), intent(in) :: highest
        integer :: binNumber
        real*8, dimension(3), intent(in) :: vel_hel
        real*8, intent(in) :: magnitude
        integer, dimension(3, NUM_OF_BINS), intent(inout) :: numberOfWDsInBin
        real*8, dimension(3, NUM_OF_BINS), intent(inout) :: sumOfVelocitiesInBin
        real*8, dimension(:, :, :), intent(inout) :: velocityArrayForMbol

        ! TODO: check if magnitude >min or <max
        binNumber = ceiling((magnitude - MBOL_MIN) / MBOL_INC)

        if (binNumber .le. NUM_OF_BINS  .AND. binNumber .ge. 1) then
            if (highest .ne. "x") then
                numberOfWDsInBin(1, binNumber) &
                    = numberOfWDsInBin(1, binNumber) + 1
                sumOfVelocitiesInBin(1, binNumber) &
                    = sumOfVelocitiesInBin(1, binNumber) + vel_hel(1)
                velocityArrayForMbol(1, binNumber, numberOfWDsInBin(1, binNumber)) &
                    = vel_hel(1)
            end if
            if (highest .ne. "y") then
                numberOfWDsInBin(2, binNumber) &
                    = numberOfWDsInBin(2, binNumber) + 1
                sumOfVelocitiesInBin(2, binNumber) &
                    = sumOfVelocitiesInBin(2, binNumber) + vel_hel(2)
                velocityArrayForMbol(2, binNumber, numberOfWDsInBin(2, binNumber)) &
                    = vel_hel(2)
            end if
            if (highest .ne. "z") then
                numberOfWDsInBin(3, binNumber) &
                    = numberOfWDsInBin(3, binNumber) + 1
                sumOfVelocitiesInBin(3, binNumber) &
                    = sumOfVelocitiesInBin(3, binNumber) + vel_hel(3)
                velocityArrayForMbol(3, binNumber, numberOfWDsInBin(3, binNumber)) &
                    = vel_hel(3)
            end if
        end if
        
    end subroutine fillSelectedDataForMbolBins


    subroutine fillFullDataForMbolBins(vel_hel, &
                                       magnitude, &
                                       numberOfWDsInBin, &
                                       sumOfVelocitiesInBin, &
                                       velocityArrayForMbol)
        
        integer :: binNumber
        real*8, dimension(3), intent(in) :: vel_hel
        real*8, intent(in) :: magnitude
        integer, dimension(NUM_OF_BINS), intent(inout) :: numberOfWDsInBin
        real*8, dimension(3, NUM_OF_BINS), intent(inout) :: sumOfVelocitiesInBin
        real*8, dimension(:, :, :), intent(inout) :: velocityArrayForMbol

        binNumber = ceiling((magnitude - MBOL_MIN) / MBOL_INC)
        if (binNumber .le. NUM_OF_BINS .AND. binNumber .ge. 1) then
            numberOfWDsInBin(binNumber) &
                = numberOfWDsInBin(binNumber) + 1
            sumOfVelocitiesInBin(:, binNumber) &
                = sumOfVelocitiesInBin(:, binNumber) + vel_hel
            velocityArrayForMbol(:, binNumber, numberOfWDsInBin(binNumber)) &
                = vel_hel
        end if
    end subroutine fillFullDataForMbolBins


    subroutine writeSelectedDataForMbolBins(sumOfVelocitiesInBin, &
                                            numberOfWDsInBin, &
                                            velocityArrayForMbol, &
                                            unitMbolAvgU, &
                                            unitMbolAvgV, &
                                            unitMbolAvgW)

        real*8, dimension(3, NUM_OF_BINS), intent(in) :: sumOfVelocitiesInBin
        integer, dimension(3, NUM_OF_BINS), intent(in) :: numberOfWDsInBin
        real*8, dimension(:, :, :), intent(in) :: velocityArrayForMbol
        integer, intent(in) :: unitMbolAvgU, &
                               unitMbolAvgV, &
                               unitMbolAvgW
        integer :: binNumber, i, j
        real*8, dimension(3) :: averageVelocityInBin
        real*8, dimension(3) :: sumOfRestsSquared
        real*8, dimension(3, NUM_OF_BINS) :: magnitudeSigma
        character(len = *), parameter :: MBOL_AVG_FORMAT = '(7(f12.6,3x))'

        do binNumber = 1, NUM_OF_BINS
            
            plots: do j =1, 3
                
                if (numberOfWDsInBin(j, binNumber) .gt. 0) then
            
                    averageVelocityInBin(j) = sumOfVelocitiesInBin(j, binNumber) &
                                           / dfloat(numberOfWDsInBin(j, binNumber))
                    
                    do i = 1, numberOfWDsInBin(j, binNumber)
                        sumOfRestsSquared(j) = sumOfRestsSquared(j) &
                                         + (velocityArrayForMbol(j, binNumber, i) &
                                         - averageVelocityInBin(j)) ** 2
                    end do
    
                    if (numberOfWDsInBin(j, binNumber) .ne. 1) then
                        magnitudeSigma(j, binNumber) &
                            = (sumOfRestsSquared(j) &
                                / dfloat(numberOfWDsInBin(j, binNumber)) &
                               - 1.d0) ** 0.5d0
                    else
                        magnitudeSigma(j, binNumber) = 100.d0
                    end if
    
                end if
            end do plots

            if (numberOfWDsInBin(1, binNumber) .gt. 0) then
                write(unitMbolAvgU, MBOL_AVG_FORMAT) &
                    MBOL_MIN + MBOL_INC*(dfloat(binNumber) - 0.5d0), &
                    averageVelocityInBin(1), &
                    magnitudeSigma(1, binNumber)
            end if
            if (numberOfWDsInBin(2, binNumber) .gt. 0) then
                write(unitMbolAvgV, MBOL_AVG_FORMAT) &
                    MBOL_MIN + MBOL_INC*(dfloat(binNumber) - 0.5d0), &
                    averageVelocityInBin(2), &
                    magnitudeSigma(2, binNumber)
            end if
            if (numberOfWDsInBin(3, binNumber) .gt. 0) then
                write(unitMbolAvgW, MBOL_AVG_FORMAT) &
                    MBOL_MIN + MBOL_INC*(dfloat(binNumber) - 0.5d0), &
                    averageVelocityInBin(3), &
                    magnitudeSigma(3, binNumber)
            end if

            sumOfRestsSquared = 0

        end do
        
    end subroutine writeSelectedDataForMbolBins


    subroutine writeFullDataForMbolBins(sumOfVelocitiesInBin, &
                                        numberOfWDsInBin, &
                                        velocityArrayForMbol, &
                                        unitMbolAvg)

        real*8, dimension(3, NUM_OF_BINS), intent(in) :: sumOfVelocitiesInBin
        integer, dimension(NUM_OF_BINS), intent(in) :: numberOfWDsInBin
        real*8, dimension(:, :, :), intent(in) :: velocityArrayForMbol
        integer, intent(in) :: unitMbolAvg
        integer :: binNumber, i
        real*8, dimension(3) :: averageVelocityInBin
        real*8, dimension(3) :: sumOfRestsSquared
        real*8, dimension(3, NUM_OF_BINS) :: magnitudeSigma
        character(len = *), parameter :: MBOL_AVG_FORMAT = '(7(f12.6,3x))'

        do binNumber = 1, NUM_OF_BINS
            
            if (numberOfWDsInBin(binNumber) > 0) then
                averageVelocityInBin(:) = sumOfVelocitiesInBin(:, binNumber) &
                                       / dfloat(numberOfWDsInBin(binNumber))
                do i = 1, numberOfWDsInBin(binNumber)
                    sumOfRestsSquared(:) = sumOfRestsSquared(:) &
                                        + (velocityArrayForMbol(:, binNumber, i) &
                                        - averageVelocityInBin(:)) ** 2
                end do
                if (numberOfWDsInBin(binNumber) .ne. 1) then
                    magnitudeSigma(:, binNumber) &
                        = (sumOfRestsSquared(:) &
                            / dfloat(numberOfWDsInBin(binNumber)) &
                           - 1.d0) ** 0.5d0
                else
                    magnitudeSigma(:, binNumber) = 100.d0
                end if
    
                write(unitMbolAvg, MBOL_AVG_FORMAT) &
                    MBOL_MIN + MBOL_INC*(dfloat(binNumber) - 0.5d0), &
                    averageVelocityInBin, &
                    magnitudeSigma(:, binNumber)
                sumOfRestsSquared = 0
            end if
        end do
        
    end subroutine writeFullDataForMbolBins


end module plot_mbol
