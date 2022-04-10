module mod_ray 
    implicit none 

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

    logical pure function hit_sphere(self, center, radius)
        real(8), intent(in) :: center(3)
        real, intent(in) :: radius
        class(Ray), intent(in) :: self
        real(8) :: a, b, c, discriminant, oc(3)

        oc = self % origin - center
        a = dot_product(self % direction, self % direction)
        b = 2.0 * dot_product(oc, self % direction)
        c = dot_product(oc, oc) - radius * radius
        discriminant = b*b - 4*a*c
        hit_sphere = discriminant > 0 
    end function hit_sphere

end module mod_ray

