module mod_ray 
    implicit none 

    private 
    public Ray

    type :: Ray 
        real(8) :: origin(3)
        real(8) :: direction(3) 
    contains 
        procedure :: at
        procedure :: hit_sphere
    end type Ray

contains 

    pure function at(self, t) result(res)
        class(Ray), intent(in) :: self 
        real(8), intent(in) :: t
        real(8) :: res(3)
        res = self%origin + t * self%direction
    end function at 

    real(8) pure function hit_sphere(self, center, radius)
        real(8), intent(in) :: center(3)
        real, intent(in) :: radius
        class(Ray), intent(in) :: self
        real(8) :: a, half_b, c, discriminant, oc(3)

        oc = self % origin - center 
        a = dot_product(self % direction, self % direction)
        half_b = dot_product(oc, self % direction)
        c = dot_product(oc, oc) - radius**2
        discriminant = half_b**2 - a*c

        if (discriminant < 0) then 
            hit_sphere = -1.0
        else 
            hit_sphere = (-half_b - sqrt(discriminant)) / a
        end if 
    end function hit_sphere

end module mod_ray

