! This module treats data from population synthesis code and applies
! criterion from Limoges et al. 2015
module synthetic
    

    use math, only: calculateStandartDeviation
    use files, only: getNumberOfLines, &
                     getNewUnit
    use commons, only: INPUT_PATH, &
                       OUTPUT_FORMAT
    use astronomy, only: convertEquatorToGalact, &
                         convertGalacticToXYZ
    use plot_uvw, only: fillSelectedDataForUVWPlot, &
                        fillFullDataForUVWPlot
    use plot_mbol, only: fillSelectedDataForMbolCloud, &
                         fillSelectedDataForMbolBins, &
                         fillFullDataForMbolBins, &
                         writeSelectedDataForMbolBins, &
                         writeFullDataForMbolBins
    
    implicit none

    public :: treatSynthData


contains

    
    subroutine treatSynthData(limogesCriterionIsUsed)

        logical, intent(in) :: limogesCriterionIsUsed
        integer :: numberOfWDs
        ! In this array we record velocity components for each WD
        ! in order to calculate standart deviation 
        real*8, dimension(:, :), allocatable :: velocityArray
        ! I/O units
        integer :: unitIn, &  ! Input data
                   unitVW, &  ! Outputs with velocities
                   unitUW, &
                   unitUV
        integer :: i
        real*8, dimension(22) :: inputData
        real*8 :: rightAscension, &  
                  declination, &
                  distanceInKpc
        real*8, dimension(3) :: vel_hel, &  ! Heliocentric velocity
                                coordOfWD
        real*8 :: distance
        real*8 :: l, &  ! Longitude
                  b  ! Lattitude
        character(len=1) :: highestCoord
        integer, dimension(3) :: counterOfWD=(/0, 0, 0/)
        real*8, dimension(3) :: vel_sum = (/0.d0, 0.d0, 0.d0/)
        real*8, dimension(3) :: vel_avg, &
                                sigma
        real*8 :: magnitude
        integer :: unitCloud, &
                   unitCloudU, &
                   unitCloudV, &
                   unitCloudW, &
                   unitMbolAvgU, &
                   unitMbolAvgV, &
                   unitMbolAvgW
        character(len = *), parameter :: MBOL_CLOUD_PATH = './outputs&
        												    &/synth&
        												    &/no_crit&
                                                            &/mbol_cloud.dat'
        character(len = *), parameter :: MBOL_CLOUD_U_PATH = './outputs&
        												    &/synth&
        												    &/limoges&
                                                            &/mbol_cloud_u.dat'
        character(len = *), parameter :: MBOL_CLOUD_V_PATH = './outputs&
        												    &/synth&
        												    &/limoges&
                                                            &/mbol_cloud_v.dat'
        character(len = *), parameter :: MBOL_CLOUD_W_PATH = './outputs&
        												    &/synth&
        												    &/limoges&
                                                            &/mbol_cloud_w.dat'
        character(len = *), parameter :: MBOL_AVG_U_PATH = './outputs&
        												    &/synth&
        												    &/limoges&
                                                          &/mbol_avg_u.dat'
        character(len = *), parameter :: MBOL_AVG_V_PATH = './outputs&
        												    &/synth&
        												    &/limoges&
                                                          &/mbol_avg_v.dat'
        character(len = *), parameter :: MBOL_AVG_W_PATH = './outputs&
        												    &/synth&
        												    &/limoges&
                                                          &/mbol_avg_w.dat' 
        real*8, parameter :: MBOL_MIN = 5.75d0                                      
        real*8, parameter :: MBOL_MAX = 20.75d0
        real*8, parameter :: MBOL_INC = 0.5d0
        integer, parameter :: NUM_OF_BINS = int((MBOL_MAX - MBOL_MIN) &
                                                / MBOL_INC)
        integer, dimension(3, NUM_OF_BINS) :: numberOfWDsInBin = 0
        real*8, dimension(3, NUM_OF_BINS) :: sumOfVelocitiesInBin = 0.d0
        real*8, dimension(:, :, :), allocatable :: velocityArrayForMbol
        character(len = *), parameter :: CLOUD_FORMAT = '(4(f12.6,3x))'
        integer, dimension(NUM_OF_BINS) :: numberOfWDsInBin_f = 0
        ! character(len = *), parameter :: UVW_PATH = './outputs/synth/no_crit/uvw.dat'
        character(len = *), parameter :: VW_PATH = './outputs/synth/limoges/vw.dat'
        character(len = *), parameter :: UW_PATH = './outputs/synth/limoges/uw.dat'
        character(len = *), parameter :: UV_PATH = './outputs/synth/limoges/uv.dat'


        print*, "Data from population synthesis code"

        numberOfWDs = getNumberOfLines(INPUT_PATH)
        print*, "Number of White Dwarfs:", numberOfWDs

        allocate(velocityArray(3, numberOfWDs))
        allocate(velocityArrayForMbol(3, NUM_OF_BINS, numberOfWDs))
        
        open(getNewUnit(unitIn), file = INPUT_PATH, status='old')
        open(getNewUnit(unitVW), file = VW_PATH, status='old')
        open(getNewUnit(unitUW), file = UW_PATH, status='old')
        open(getNewUnit(unitUV), file = UV_PATH, status='old')
        open(getNewUnit(unitCloud), file = MBOL_CLOUD_PATH, status='old')
        open(getNewUnit(unitCloudU), file = MBOL_CLOUD_U_PATH, status='old')
        open(getNewUnit(unitCloudV), file = MBOL_CLOUD_V_PATH, status='old')
        open(getNewUnit(unitCloudW), file = MBOL_CLOUD_W_PATH, status='old')
        open(getNewUnit(unitMbolAvgU), file = MBOL_AVG_U_PATH, status='old')
        open(getNewUnit(unitMbolAvgV), file = MBOL_AVG_V_PATH, status='old')
        open(getNewUnit(unitMbolAvgW), file = MBOL_AVG_W_PATH, status='old')

        if (limogesCriterionIsUsed .eqv. .true.) then
            print*, "Limoges criterion is used."
            
            do i = 1, numberOfWDs

                read(unitIn, *) inputData

                magnitude = inputData(4)
                rightAscension = inputData(10)
                declination = inputData(11)
                distanceInKpc = inputData(12)
                vel_hel = inputData(20:22)
                
                distance = distanceInKpc * 1.d3  ! Converting kpc to pc
              
                ! Converting Right Ascension and Declination to lattitude 
                ! and longitude
                call convertEquatorToGalact(rightAscension, &
                                            declination, &
                                            l, &
                                            b) 
            
                ! Converting lattitude and longitude to X,Y,Z
                coordOfWD = convertGalacticToXYZ(distance, l, b)

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
            
            end do
        
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
        else 
            print*, "Limoges criterion is not used"
   
            do i = 1, numberOfWDs
                read(unitIn,*) inputData 
                magnitude = inputData(4)
                vel_hel = inputData(20:22)
                ! Summing velocitiy components
                vel_sum = vel_sum + vel_hel
                ! Filling array for calculating SD
                velocityArray(:, i) = vel_hel

                write(unitCloud, CLOUD_FORMAT) magnitude, vel_hel

                call fillFullDataForMbolBins(vel_hel, &
                                             magnitude, &
                                             numberOfWDsInBin_f, &
                                             sumOfVelocitiesInBin, &
                                             velocityArrayForMbol)
            end do 

            call calculateStandartDeviation(vel_sum, &
                                            numberOfWDs, &
                                            velocityArray, &
                                            vel_avg, &
                                            sigma)
        end if

        write(6,*) 'Average relative to Sun:', vel_avg
        write(6,*) 'Sigmas:                 ', sigma
    
        deallocate(velocityArray)

    end subroutine


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
