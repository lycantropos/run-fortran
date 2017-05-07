module astronomy
    use, intrinsic :: iso_fortran_env, dp=>real64
    use math, only: PI, &
                    multiplyMatrixByVector
    implicit none

    private 
    
    public :: CTK, &
              TRAN_MATR, &
              convertGalacticToXYZ, &
              convertEquatorToGalact, &
              convertHoursToRad, &
              convertDegreesToRad, &
              convertEquatorMotionToUVW
              
    ! Astronomical unit in km/s
    real*8, parameter :: CTK = 4.74047d0
    ! Transformation matrix from equatorial to galactic coordinates
    real*8, parameter, dimension(3, 3) :: TRAN_MATR &
        = reshape( (/-0.054875d0,-0.873437d0,-0.483835d0,&       
                      0.494109d0,-0.444829d0, 0.746982d0,&       
                     -0.867666d0,-0.198076d0, 0.455983d0/),&     
                     shape(TRAN_MATR), order = (/2,1/))
    ! Right ascension of Galactic pole
    real*8, parameter :: RA_GPOLE = 192.859508d0 * PI / 180.d0 
    ! Declination of Galactic pole
    real*8, parameter :: DEC_GPOLE = 27.128336d0 * PI /180.d0 
    ! Auxiliary angle
    real*8, parameter :: AUX_ANGLE = 122.932d0 * PI / 180.d0 

contains
    
    function convertGalacticToXYZ(r, l, b) result(coordinate)
        real*8, intent(in) :: r, l, b
        real*8, dimension(3) :: coordinate

        coordinate(1) = r * dcos(b) * dcos(l)
        coordinate(2) = r * dcos(b) * dsin(l)
        coordinate(3) = r * dsin(b)
    end function


    elemental function convertHoursToRad(angleInHours) result(angleInRadians)
        character(len = 11), intent(in) :: angleInHours
        real*8 angleInRadians
        real*8 hours,minutes,seconds
        
        read(angleInHours(1:2), *) hours
        read(angleInHours(4:5), *) minutes
        read(angleInHours(7:11), *) seconds
        angleInRadians = (hours+(minutes+seconds/60.d0)/60.d0) * 15.d0 & 
                         * pi / 180.d0
    end function convertHoursToRad


    elemental function convertDegreesToRad(angleInDegrees) result(angleInRadians)
        character(len = 11), intent(in) :: angleInDegrees
        real*8 angleInRadians
        real*8 degrees, arcmins, arcsecs
        
        read(angleInDegrees(1:2), *) degrees
        read(angleInDegrees(4:5), *) arcmins
        read(angleInDegrees(7:11), *) arcsecs
        angleInRadians = (degrees+(arcmins+arcsecs/60.d0)/60.d0) * pi / 180.d0
    end function convertDegreesToRad


    subroutine convertEquatorToGalact(ra,dec,l,b)
        real*8,intent(in) :: ra,dec
        real*8,intent(out) :: l,b                 
        real*8 x,y

        !$b=arcsin\Big(cos(\delta)cos(\delta_{G})cos(\alpha-\alpha_{G})+\\+sin(\delta)sin(\delta_{G})\Big)$
        b = (dasin(dcos(dec)*dcos(DEC_GPOLE)*dcos(ra-RA_GPOLE)&
            + dsin(dec)*dsin(DEC_GPOLE))) !lattitude 
        x = dsin(dec) - dsin(b)*dsin(DEC_GPOLE)
        y = dcos(dec) * dsin(ra-RA_GPOLE) * dcos(DEC_GPOLE)
        l = datan(x/y) + AUX_ANGLE - PI/2.d0 !longitude    
        if (x.gt.0.d0 .AND. y.lt.0.d0) then
            l = l + PI
        else if (x.lt.0.d0 .AND. y.lt.0.d0) then
            l = l + PI
        else if (x.lt.0.d0 .AND. y.gt.0.d0) then
            l = l + 2.d0*PI
        else if (l .gt. 2.d0*PI) then 
            l = l - 2.d0*PI
        end if
    end subroutine


    subroutine convertEquatorMotionToUVW(rightAscension, &
                                         declination, &
                                         distance, &
                                         motionInDEC, &
                                         motionInRA, &
                                         vel_hel)
        real*8, intent(in) :: rightAscension, &
                              declination, &
                              distance, &
                              motionInDEC, &
                              motionInRA
        real*8, dimension(3), intent(out) :: vel_hel
        real(dp), parameter :: VEL_RAD = 0.d0
        real(dp) :: motionInRAAster
        real(dp), dimension(3) :: vel_motion = (/VEL_RAD, 0.d0, 0.d0/)
        real*8, dimension(3, 3) :: rotationMatrix

        ! Motion in right ascension with asterisk
        motionInRAAster = motionInRA * dcos(declination)          
        
        vel_motion(2) = CTK * motionInRAAster * distance              
        vel_motion(3) = CTK * motionInDEC * distance
        
        call fillRotationMatrix(rightAscension, declination, rotationMatrix)         
        
        ! U,V,W         
        vel_hel = multiplyMatrixByVector(TRAN_MATR, &
                        multiplyMatrixByVector(rotationMatrix,&
                                                       vel_motion))
    end subroutine convertEquatorMotionToUVW


    subroutine fillRotationMatrix(RA, DEC, A)

        real*8, intent(in) :: RA, &
                              DEC
        real*8, dimension(3, 3), intent(out) :: A
        A(1,1) = dcos(RA) * dcos(DEC)  
        A(2,1) = dsin(RA) * dcos(DEC)  
        A(3,1) = dsin(DEC)        

        A(1,2) = - dsin(RA)
        A(2,2) = dcos(RA)
        A(3,2) = 0.d0

        A(1,3) = - dcos(RA)*dsin(DEC)
        A(2,3) = - dsin(RA)*dsin(DEC)
        A(3,3) = dcos(DEC)

    end subroutine


end module