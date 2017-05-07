module plot_mbol

    use, intrinsic :: iso_fortran_env, dp=>real64
    use derived_types, only: Star, &
                             JaggedArray
    use files, only: getNewUnit
    use math, only: getSD

    implicit none

    private

    public :: plotUVWvsMbol

    real(dp), parameter :: MBOL_MIN = 5.75_dp
    real(dp), parameter :: MBOL_MAX = 20.75_dp
    real(dp), parameter :: MBOL_INC = 0.5_dp
    integer, parameter :: NUM_OF_BINS = int((MBOL_MAX - MBOL_MIN) &
                                            / MBOL_INC)
    character(len = *), parameter :: CLOUD_U_FORMAT = '(2(f12.6,3x))'
    character(len = *), parameter :: CLOUD_FORMAT = '(4(f12.6,3x))'
    character(len = *), parameter :: MBOL_AVG_FORMAT = '(7(f12.6,3x))'
    character(len = *), parameter :: MBOL_CLOUD_OBS_U_PATH &
        = './outputs/observ/limoges/mbol_cloud_u.dat'
    character(len = *), parameter :: MBOL_CLOUD_SYN_U_PATH &
        = './outputs/synth/limoges/mbol_cloud_u.dat'
    character(len = *), parameter :: MBOL_CLOUD_OBS_V_PATH &
        = './outputs/observ/limoges/mbol_cloud_v.dat'
    character(len = *), parameter :: MBOL_CLOUD_SYN_V_PATH &
        = './outputs/synth/limoges/mbol_cloud_v.dat'
    character(len = *), parameter :: MBOL_CLOUD_OBS_W_PATH &
        = './outputs/observ/limoges/mbol_cloud_w.dat'
    character(len = *), parameter :: MBOL_CLOUD_SYN_W_PATH &
        = './outputs/synth/limoges/mbol_cloud_w.dat'
    character(len = *), parameter :: MBOL_AVG_OBS_U_PATH &
        = './outputs/observ/limoges/mbol_avg_u.dat'
    character(len = *), parameter :: MBOL_AVG_SYN_U_PATH &
        = './outputs/synth/limoges/mbol_avg_u.dat'
    character(len = *), parameter :: MBOL_AVG_OBS_V_PATH &
        = './outputs/observ/limoges/mbol_avg_v.dat'
    character(len = *), parameter :: MBOL_AVG_SYN_V_PATH &
        = './outputs/synth/limoges/mbol_avg_v.dat'
    character(len = *), parameter :: MBOL_AVG_OBS_W_PATH &
        = './outputs/observ/limoges/mbol_avg_w.dat'
    character(len = *), parameter :: MBOL_AVG_SYN_W_PATH &
        = './outputs/synth/limoges/mbol_avg_w.dat'
    character(len = *), parameter :: MBOL_CLOUD_OBS_PATH &
        = './outputs/observ/no_crit/mbol_cloud.dat'
    character(len = *), parameter :: MBOL_CLOUD_SYN_PATH &
        = './outputs/synth/no_crit/mbol_cloud.dat'
    character(len = *), parameter :: MBOL_AVG_OBS_PATH &
        = './outputs/observ/no_crit/mbol_avg.dat'
    character(len = *), parameter :: MBOL_AVG_SYN_PATH &
        = './outputs/synth/no_crit/mbol_avg.dat'

    interface plotUVWvsMbol
            module procedure threeSamples_plotUVWvsMbol
            module procedure oneSample_plotUVWvsMbol
    end interface plotUVWvsMbol                                                                                                        


contains

    
    subroutine oneSample_plotUVWvsMbol(sample, dataIsUsed)
        type (Star), dimension(:), intent(in) :: sample
        character(len = *), intent(in) :: dataIsUsed
        integer :: i
        integer :: unitCloud
        type (JaggedArray), dimension(:), allocatable :: bins
        integer, dimension(NUM_OF_BINS) :: wdInBinCounter = 0
        integer :: binNumber
        integer :: unitMbolAvg
        character(len = :), allocatable :: mbol_cloud_path
        character(len = :), allocatable :: mbol_avg_path

        select case (dataIsUsed)
        case("observational")
            mbol_cloud_path = MBOL_CLOUD_OBS_PATH
            mbol_avg_path = MBOL_AVG_OBS_PATH
        case("synthetic")
            mbol_cloud_path = MBOL_CLOUD_SYN_PATH
            mbol_avg_path = MBOL_AVG_SYN_PATH
        case default
            print *, "Error: case default in oneSample_plotUVWvsMbol"
            stop
        end select

        ! Filing files with cloud data
        open(getNewUnit(unitCloud), file = mbol_cloud_path, status='old')
        do i = 1, size(sample)
            write(unitCloud, CLOUD_FORMAT) sample(i)%magnitude, &
                                           sample(i)%vel(:)
        end do
        ! Filling bins
        allocate(bins(NUM_OF_BINS))
        do i = 1, size(sample)
            if (sample(i)%magnitude > MBOL_MIN .and. &
                    sample(i)%magnitude < MBOL_MAX) then
                binNumber = ceiling((sample(i)%magnitude - MBOL_MIN) &
                                    / MBOL_INC)
                wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
            end if
        end do
        do i = 1, NUM_OF_BINS
            if (wdInBinCounter(i) > 0) then
                allocate(bins(i)%row(wdInBinCounter(i)))
            end if
        end do
        wdInBinCounter = 0
        do i = 1, size(sample)
            if (sample(i)%magnitude > MBOL_MIN .and. &
                    sample(i)%magnitude < MBOL_MAX) then
                binNumber = ceiling((sample(i)%magnitude - MBOL_MIN) &
                                    / MBOL_INC)
                wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                bins(binNumber)%row(wdInBinCounter(binNumber)) = sample(i)
            end if
        end do
        open(getNewUnit(unitMbolAvg), file = mbol_avg_path, status='old')
        do i = 1, NUM_OF_BINS
            if (allocated(bins(i)%row)) then
                write(unitMbolAvg, MBOL_AVG_FORMAT) &
                    MBOL_MIN + MBOL_INC*(dfloat(i) - 0.5_dp), &
                    sum(bins(i)%row(:)%vel(1)) &
                        / size(bins(i)%row), &
                    sum(bins(i)%row(:)%vel(2)) &
                        / size(bins(i)%row), &
                    sum(bins(i)%row(:)%vel(3)) &
                        / size(bins(i)%row), &
                    getSD(bins(i)%row(:)%vel(1)), &
                    getSD(bins(i)%row(:)%vel(2)), &
                    getSD(bins(i)%row(:)%vel(3))
            end if
        end do
    end subroutine oneSample_plotUVWvsMbol


    subroutine threeSamples_plotUVWvsMbol(sampleUvsMbol, &
                                          sampleVvsMbol, &
                                          sampleWvsMbol, &
                                          dataIsUsed)
        type (Star), dimension(:), allocatable, intent(in) :: sampleUvsMbol
        type (Star), dimension(:), allocatable, intent(in) :: sampleVvsMbol
        type (Star), dimension(:), allocatable, intent(in) :: sampleWvsMbol
        character(len = *), intent(in) :: dataIsUsed
        integer :: i
        integer :: unitCloudU, &
                   unitCloudV, &
                   unitCloudW
        type (JaggedArray), dimension(:), allocatable :: binsUvsMbol
        type (JaggedArray), dimension(:), allocatable :: binsVvsMbol
        type (JaggedArray), dimension(:), allocatable :: binsWvsMbol
        integer, dimension(NUM_OF_BINS) :: wdInBinCounter = 0
        integer :: binNumber
        integer :: unitMbolAvgU, &
                   unitMbolAvgV, &
                   unitMbolAvgW
        character(len = :), allocatable :: mbol_cloud_u_path
        character(len = :), allocatable :: mbol_cloud_v_path
        character(len = :), allocatable :: mbol_cloud_w_path
        character(len = :), allocatable :: mbol_avg_u_path
        character(len = :), allocatable :: mbol_avg_v_path
        character(len = :), allocatable :: mbol_avg_w_path

        select case (dataIsUsed)
        case("observational")
            mbol_cloud_u_path = MBOL_CLOUD_OBS_U_PATH
            mbol_cloud_v_path = MBOL_CLOUD_OBS_V_PATH
            mbol_cloud_w_path = MBOL_CLOUD_OBS_W_PATH
            mbol_avg_u_path = MBOL_AVG_OBS_U_PATH
            mbol_avg_v_path = MBOL_AVG_OBS_V_PATH
            mbol_avg_w_path = MBOL_AVG_OBS_W_PATH
        case("synthetic")
            mbol_cloud_u_path = MBOL_CLOUD_SYN_U_PATH
            mbol_cloud_v_path = MBOL_CLOUD_SYN_V_PATH
            mbol_cloud_w_path = MBOL_CLOUD_SYN_W_PATH
            mbol_avg_u_path = MBOL_AVG_SYN_U_PATH
            mbol_avg_v_path = MBOL_AVG_SYN_V_PATH
            mbol_avg_w_path = MBOL_AVG_SYN_W_PATH
        case default
            print *, "Error: case default in oneSample_plotUVWvsMbol"
            stop
        end select

        if (allocated(sampleUvsMbol) .and. allocated(sampleVvsMbol) &
                                     .and. allocated(sampleWvsMbol)) then
            ! Filing files with cloud data
            open(getNewUnit(unitCloudU), file = mbol_cloud_u_path, status='old')
            do i = 1, size(sampleUvsMbol)
                write(unitCloudU, CLOUD_U_FORMAT) sampleUvsMbol(i)%magnitude, &
                                                sampleUvsMbol(i)%vel(1)
            end do

            open(getNewUnit(unitCloudV), file = mbol_cloud_v_path, status='old')
            do i = 1, size(sampleVvsMbol)
                write(unitCloudV, CLOUD_U_FORMAT) sampleVvsMbol(i)%magnitude, &
                                                sampleVvsMbol(i)%vel(1)
            end do

            open(getNewUnit(unitCloudW), file = mbol_cloud_w_path, status='old')
            do i = 1, size(sampleWvsMbol)
                write(unitCloudW, CLOUD_U_FORMAT) sampleWvsMbol(i)%magnitude, &
                                                sampleWvsMbol(i)%vel(1)
            end do

            ! Filling bins 
            ! U vs Mbol
            allocate(binsUvsMbol(NUM_OF_BINS))
            do i = 1, size(sampleUvsMbol)
                if (sampleUvsMbol(i)%magnitude > MBOL_MIN .and. &
                    sampleUvsMbol(i)%magnitude < MBOL_MAX) then
                    binNumber = ceiling((sampleUvsMbol(i)%magnitude - MBOL_MIN) &
                                    / MBOL_INC)
                    wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                end if
            end do
            do i = 1, NUM_OF_BINS
                if (wdInBinCounter(i) > 0) then
                    allocate(binsUvsMbol(i)%row(wdInBinCounter(i)))
                end if
            end do
            wdInBinCounter = 0
            do i = 1, size(sampleUvsMbol)
                if (sampleUvsMbol(i)%magnitude > MBOL_MIN .and. &
                    sampleUvsMbol(i)%magnitude < MBOL_MAX) then
                    binNumber = ceiling((sampleUvsMbol(i)%magnitude - MBOL_MIN) &
                                         / MBOL_INC)
                    wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                    binsUvsMbol(binNumber)%row(wdInBinCounter(binNumber)) = sampleUvsMbol(i)
                end if
            end do
            open(getNewUnit(unitMbolAvgU), file = mbol_avg_u_path, status='old')
            do i = 1, NUM_OF_BINS
                if (allocated(binsUvsMbol(i)%row)) then
                    write(unitMbolAvgU, MBOL_AVG_FORMAT) &
                        MBOL_MIN + MBOL_INC*(dfloat(i) - 0.5_dp), &
                        sum(binsUvsMbol(i)%row(:)%vel(1)) &
                            / size(binsUvsMbol(i)%row), &
                        getSD(binsUvsMbol(i)%row(:)%vel(1))
                end if
            end do

            ! V vs Mbol
            wdInBinCounter = 0
            allocate(binsVvsMbol(NUM_OF_BINS))
            do i = 1, size(sampleVvsMbol)
                if (sampleVvsMbol(i)%magnitude > MBOL_MIN .and. &
                        sampleVvsMbol(i)%magnitude < MBOL_MAX) then
                    binNumber = ceiling((sampleVvsMbol(i)%magnitude - MBOL_MIN) &
                                     / MBOL_INC)
                    wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                end if
            end do
            do i = 1, NUM_OF_BINS
                if (wdInBinCounter(i) > 0) then
                    allocate(binsVvsMbol(i)%row(wdInBinCounter(i)))
                end if
            end do
            wdInBinCounter = 0
            do i = 1, size(sampleVvsMbol)
                if (sampleVvsMbol(i)%magnitude > MBOL_MIN .and. &
                    sampleVvsMbol(i)%magnitude < MBOL_MAX) then
                    binNumber = ceiling((sampleVvsMbol(i)%magnitude - MBOL_MIN) &
                                        / MBOL_INC)
                    wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                    binsVvsMbol(binNumber)%row(wdInBinCounter(binNumber)) = sampleVvsMbol(i)
                end if
            end do
            open(getNewUnit(unitMbolAvgV), file = mbol_avg_v_path, status='old')
            do i = 1, NUM_OF_BINS
                if (allocated(binsVvsMbol(i)%row)) then
                    write(unitMbolAvgV, MBOL_AVG_FORMAT) &
                        MBOL_MIN + MBOL_INC*(dfloat(i) - 0.5_dp), &
                        sum(binsVvsMbol(i)%row(:)%vel(2)) &
                            / size(binsVvsMbol(i)%row), &
                        getSD(binsVvsMbol(i)%row(:)%vel(2))
                end if
            end do

            ! W vs Mbol
            wdInBinCounter = 0
            allocate(binsWvsMbol(NUM_OF_BINS))
            do i = 1, size(sampleWvsMbol)
                if (sampleWvsMbol(i)%magnitude > MBOL_MIN .and. &
                    sampleWvsMbol(i)%magnitude < MBOL_MAX) then
                    binNumber = ceiling((sampleWvsMbol(i)%magnitude - MBOL_MIN) &
                                        / MBOL_INC)
                    ! print*, i, binNumber,sampleWvsMbol(i)%magnitude
                    wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                end if
            end do
            do i = 1, NUM_OF_BINS
                if (wdInBinCounter(i) > 0) then
                    allocate(binsWvsMbol(i)%row(wdInBinCounter(i)))
                end if
            end do
            wdInBinCounter = 0
            do i = 1, size(sampleWvsMbol)
                if (sampleWvsMbol(i)%magnitude > MBOL_MIN .and. &
                        sampleWvsMbol(i)%magnitude < MBOL_MAX) then
                    binNumber = ceiling((sampleWvsMbol(i)%magnitude - MBOL_MIN) &
                                        / MBOL_INC)
                    wdInBinCounter(binNumber) = wdInBinCounter(binNumber) + 1
                    binsWvsMbol(binNumber)%row(wdInBinCounter(binNumber)) = sampleWvsMbol(i)
                end if
            end do
            open(getNewUnit(unitMbolAvgW), file = mbol_avg_w_path, status='old')
            do i = 1, NUM_OF_BINS
                if (allocated(binsWvsMbol(i)%row)) then
                    write(unitMbolAvgW, MBOL_AVG_FORMAT) &
                        MBOL_MIN + MBOL_INC*(dfloat(i) - 0.5_dp), &
                        sum(binsWvsMbol(i)%row(:)%vel(3)) &
                            / size(binsWvsMbol(i)%row), &
                        getSD(binsWvsMbol(i)%row(:)%vel(3))
                end if
            end do
        end if
    end subroutine threeSamples_plotUVWvsMbol
end module plot_mbol
