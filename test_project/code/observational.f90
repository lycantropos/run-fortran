! This module treats observational data
module observational   


    use, intrinsic :: iso_fortran_env
    use files, only: getNumberOfLines, &
                     getNewUnit
    ! TODO: hide this in plot modules
    use commons, only: OUTPUT_FORMAT
    use astronomy, only: convertHoursToRad, &
                         convertDegreesToRad, &
                         convertEquatorToGalact, &
                         convertGalacticToXYZ, &
                         convertEquatorMotionToUVW
    use math, only: calculateStandartDeviation
    use plot_uvw, only: fillSelectedDataForUVWPlot, &
                        fillFullDataForUVWPlot
    use plot_mbol, only: fillSelectedDataForMbolCloud, &
                         fillSelectedDataForMbolBins, &
                         fillFullDataForMbolBins, &
                         writeSelectedDataForMbolBins, &
                         writeFullDataForMbolBins

    implicit none

    private :: OBSERV_PATH, &
               MBOL_CLOUD_PATH, &
               MBOL_AVG_PATH, &
               ! TODO: hide this in plot_mbol
               MBOL_MIN, &
               MBOL_MAX, &
               MBOL_INC, &
               NUM_OF_BINS, &
               CLOUD_FORMAT
        character(len = *), parameter :: OBSERV_PATH = './inputs&
                                                        &/observational.dat'
        character(len = *), parameter :: MBOL_CLOUD_PATH = './outputs&
        												    &/observ&
        												    &/no_crit&
                                                            &/mbol_cloud.dat'
        character(len = *), parameter :: MBOL_AVG_PATH = './outputs&
        												    &/observ&
        												    &/no_crit&
                                                          &/mbol_avg.dat'
        character(len = *), parameter :: MBOL_CLOUD_U_PATH = './outputs&
        													  &/observ&
        													  &/limoges&
                                                            &/mbol_cloud_u.dat'
        character(len = *), parameter :: MBOL_CLOUD_V_PATH = './outputs&
        													  &/observ&
        													  &/limoges&
                                                            &/mbol_cloud_v.dat'
        character(len = *), parameter :: MBOL_CLOUD_W_PATH = './outputs&
        													  &/observ&
        													  &/limoges&
                                                            &/mbol_cloud_w.dat'
        character(len = *), parameter :: MBOL_AVG_U_PATH = './outputs&
        													&/observ&
        													&/limoges&
                                                          &/mbol_avg_u.dat'
        character(len = *), parameter :: MBOL_AVG_V_PATH = './outputs&
        													&/observ&
        													&/limoges&
                                                          &/mbol_avg_v.dat'
        character(len = *), parameter :: MBOL_AVG_W_PATH = './outputs&
        													&/observ&
        													&/limoges&
                                                          &/mbol_avg_w.dat'                                                          
        ! Binning parameters for bolometric magnitude (Mbol)
        ! TODO: Make an array with 0.5 step
        real*8, parameter :: MBOL_MIN = 5.75d0                                      
        real*8, parameter :: MBOL_MAX = 20.75d0
        real*8, parameter :: MBOL_INC = 0.5d0
        integer, parameter :: NUM_OF_BINS = int((MBOL_MAX - MBOL_MIN) &
                                                / MBOL_INC)
        character(len = *), parameter :: CLOUD_FORMAT = '(4(f12.6,3x))'
    
    public :: treatObservData


contains


    subroutine treatObservData(limogesCriterionIsUsed)

        logical, intent(in) :: limogesCriterionIsUsed
        integer :: numberOfWDs
        ! In this array we record velocity components for each WD
        ! in order to calculate standart deviation 
        ! TODO: hide it in plots
        real*8, dimension(:, :), allocatable :: velocityArray
        ! In this array we record velocity components for each WD
        ! in every bin of Mbol in order to calculate standart 
        ! deviations per corresponding bin 
        ! NOTE: 2nd dimension should be dynamic - numberOfWDsInBin
        real*8, dimension(:, :, :), allocatable :: velocityArrayForMbol
        ! I/O units
        integer :: unitUVW, &
                   unitVW, &  ! Outputs with velocities
                   unitUW, &
                   unitUV, &
                   ! Observational data from Limoges et al. 2015
                   unitObs, &  
                   ! Output with individual (Mbol, U, V, W) for each WD
                   unitCloud, &  
                   ! Output with average velocities in bins of Mbol
                   unitMbolAvg, &
                   unitCloudU, unitCloudV, unitCloudW, &
                   unitMbolAvgU, unitMbolAvgV, unitMbolAvgW
        integer :: i
        ! Input data
        real*8 :: distance, &
                  motionInRA, &  ! Motion in Right Ascension (RA)
                  motionInDEC, &  ! Motion in Declination (DEC)
                  magnitude
        ! Input data in special formats to be converted
        character(len = 11) RA_inHours, &
                            DEC_inDegrees
        ! Data to be obtained from RA_inHours and DEC_inDegrees         
        real*8 :: rightAscension, &
                  declination
        ! Galactic angles
        real*8 :: l, &  ! Longitude
                  b  ! Lattitude
        ! WD coordinates in X, Y, Z
        real*8, dimension(3) :: coordOfWD 
        ! Heliocentric velocity of WD
        real*8, dimension(3) :: vel_hel
        integer, dimension(3, NUM_OF_BINS) :: numberOfWDsInBin = 0
        ! NOTE: this is not nortmal at all (f = for no limoges)
        integer, dimension(NUM_OF_BINS) :: numberOfWDsInBin_f = 0
        real*8, dimension(3, NUM_OF_BINS) :: sumOfVelocitiesInBin = 0.d0
        integer, dimension(3) :: counterOfWD = 0
        real*8, dimension(3) :: vel_sum = 0.d0
        real*8, dimension(3) :: vel_avg, &
                                sigma
        character(len=1) :: highestCoord
        character(len = *), parameter :: UVW_PATH = './outputs/observ/no_crit/uvw.dat'
        character(len = *), parameter :: VW_PATH = './outputs/observ/limoges/vw.dat'
        character(len = *), parameter :: UW_PATH = './outputs/observ/limoges/uw.dat'
        character(len = *), parameter :: UV_PATH = './outputs/observ/limoges/uv.dat'

        print*, "Observational data from Limoges et al. 2015"

        numberOfWDs = getNumberOfLines(OBSERV_PATH)
        print*, "Number of White Dwarfs:", numberOfWDs
        allocate(velocityArray(3, numberOfWDs))
        allocate(velocityArrayForMbol(3, NUM_OF_BINS, numberOfWDs))
        
        open(99, file="/home/georgy/Documents/program/additional_programs&
                      &/kinematics/logs/log.out", status="old")
        open(getNewUnit(unitVW), file = VW_PATH, status='old')
        open(getNewUnit(unitUW), file = UW_PATH, status='old')
        open(getNewUnit(unitUV), file = UV_PATH, status='old')
        open(getNewUnit(unitObs), file = OBSERV_PATH, status='old')
        open(getNewUnit(unitCloud), file = MBOL_CLOUD_PATH, status='old')
        open(getNewUnit(unitMbolAvg), file = MBOL_AVG_PATH, status='old')
        open(getNewUnit(unitCloudU), file = MBOL_CLOUD_U_PATH, status='old')
        open(getNewUnit(unitCloudV), file = MBOL_CLOUD_V_PATH, status='old')
        open(getNewUnit(unitCloudW), file = MBOL_CLOUD_W_PATH, status='old')
        open(getNewUnit(unitMbolAvgU), file = MBOL_AVG_U_PATH, status='old')
        open(getNewUnit(unitMbolAvgV), file = MBOL_AVG_V_PATH, status='old')
        open(getNewUnit(unitMbolAvgW), file = MBOL_AVG_W_PATH, status='old')
        open(getNewUnit(unitUVW), file = UVW_PATH, status='old')

        if (limogesCriterionIsUsed) then

            walk_WDs: do i = 1, numberOfWDs
            
                read(unitObs, *) distance, &
                                 RA_inHours, &
                                 DEC_inDegrees, &
                                 motionInRA, &
                                 motionInDEC, &
                                 magnitude
            
                ! Сonverting angles from char to radians
                rightAscension = convertHoursToRad(RA_inHours)      
                declination = convertDegreesToRad(DEC_inDegrees)
        
                ! Converting angles to lattitude and longitude
                call convertEquatorToGalact(rightAscension, &
                                            declination, &
                                            l, &
                                            b) 
            
                ! Converting lattitude and longitude to X,Y,Z
                coordOfWD = convertGalacticToXYZ(distance, &
                                                 l, &
                                                 b)
            
                call convertEquatorMotionToUVW(rightAscension, &
                                               declination, &
                                               distance, &
                                               motionInDEC, &
                                               motionInRA, &
                                               vel_hel)
            
                highestCoord = getHighestCoord(coordOfWD)

                call fillSelectedDataForUVWPlot(highestCoord, &
                                                vel_hel, &
                                                counterOfWD, &
                                                vel_sum, &
                                                velocityArray, &
                                                unitVW, &
                                                unitUW, &
                                                unitUV)
            
                call fillSelectedDataForMbolCloud(highestCoord, &
                                                  magnitude, &
                                                  vel_hel, &
                                                  unitCloudU, &
                                                  unitCloudV, &
                                                  unitCloudW)
                
                call fillSelectedDataForMbolBins(highestCoord, &
                                                 vel_hel, &
                                                 magnitude, &
                                                 numberOfWDsInBin, &
                                                 sumOfVelocitiesInBin, &
                                                 velocityArrayForMbol)
            end do walk_WDs
            
            call writeSelectedDataForMbolBins(sumOfVelocitiesInBin, &
                                              numberOfWDsInBin, &
                                              velocityArrayForMbol, &
                                              unitMbolAvgU, &
                                              unitMbolAvgV, &
                                              unitMbolAvgW)
            
            call calculateStandartDeviation(vel_sum, &
                                            counterOfWD, &
                                            velocityArray, &
                                            vel_avg, &
                                            sigma)
            
            write(6,*) 'Average relative to Sun:', vel_avg
            write(6,*) 'Sigmas:                 ', sigma
        
        else
            
            do i = 1, numberOfWDs
                read(unitObs, *) distance, &
                                 RA_inHours, &
                                 DEC_inDegrees, &
                                 motionInRA, &
                                 motionInDEC, &
                                 magnitude

                ! Сonverting angles from char to radians
                rightAscension = convertHoursToRad(RA_inHours)      
                declination = convertDegreesToRad(DEC_inDegrees)

                call convertEquatorMotionToUVW(rightAscension, &
                                               declination, &
                                               distance, &
                                               motionInDEC, &
                                               motionInRA, &
                                               vel_hel)

                call fillFullDataForUVWPlot(vel_hel, &
                                            i, &
                                            vel_sum, &
                                            velocityArray, &
                                            unitUVW)

                write(unitCloud, CLOUD_FORMAT) magnitude, vel_hel

                call fillFullDataForMbolBins(vel_hel, &
                                             magnitude, &
                                             numberOfWDsInBin_f, &
                                             sumOfVelocitiesInBin, &
                                             velocityArrayForMbol)

            end do

            call writeFullDataForMbolBins(sumOfVelocitiesInBin, &
                                          numberOfWDsInBin_f, &
                                          velocityArrayForMbol, &
                                          unitMbolAvg)

            call calculateStandartDeviation(vel_sum, &
                                            numberOfWDs, &
                                            velocityArray, &
                                            vel_avg, &
                                            sigma)

            write(6,*) 'Average relative to Sun:', vel_avg
            write(6,*) 'Sigmas:                 ', sigma

        end if


    end subroutine treatObservData


    function getHighestCoord(coords) result(highest)

        implicit none
        real*8, dimension(3), intent(in) :: coords
        character(len = 1) :: highest
        real*8 :: highestValue

        highestValue = maxval(abs(coords))

        if (abs(highestValue - abs(coords(1))) .lt. 1.0d-5) then
            highest = "x"
        else if (abs( highestValue - abs(coords(2) )) .lt. 1.0d-5) then
            highest = "y"
        else if (abs( highestValue - abs(coords(3) )) .lt. 1.0d-5) then
            highest = "z"
        else 
            print *, "Error: couldn't determine highest coordinate"
        end if

    end function getHighestCoord


end module
