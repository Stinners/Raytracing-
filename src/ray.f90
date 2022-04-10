module mod_ray 
    implicit none 

    type :: Ray 
        real(8) :: origin(3)
        real(8) :: direction(3) 
    contains 
        procedure at
    end type Ray

contains 

    pure function at(self, t) result(res)
        class(Ray), intent(in) :: self 
        real(8), intent(in) :: t
        real(8) :: res(3)
        res = self%origin + t * self%direction
    end function at 

end module mod_ray

