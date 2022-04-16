module mod_sphere
    use mod_hittable, only: hit_record_t, hittable, material_t
    use mod_ray, only: Ray
    implicit none 

    private 
    public sphere_t

    type, extends(hittable) :: sphere_t
        real(8) :: center(3), radius
        class(material_t), pointer :: material
    contains 
        procedure :: hit
    end type sphere_t
    
contains 

    logical function hit(self, r, t_min, t_max, hit_record)
        class(sphere_t), intent(in) :: self
        type(Ray), intent(in) :: r 
        real(8), intent(in) :: t_min, t_max
        class(hit_record_t), intent(out) :: hit_record

        real(8) :: oc(3), a, half_b, c, discriminant, sqrtd, root

        oc = r % origin - self % center 
        a = dot_product(r % direction, r % direction)
        half_b = dot_product(oc, r % direction)
        c = dot_product(oc, oc) - (self % radius) ** 2 

        discriminant = half_b ** 2 - a*c
        if (discriminant < 0) then 
            hit = .false.
            return 
        end if 
        sqrtd = sqrt(discriminant)

        ! Find the nearest root in the acceptible range 
        root = (-half_b - sqrtd) / a 
        if (root < t_min .or. t_max < root) then 
            root = (-half_b + sqrtd) / a 
            if (root < t_min .or. t_max < root) then
                hit = .false.
                return 
            end if 
        end if 

        ! TODO, pull this all into a custom constructor for hit_record_t
        hit_record % t = root 
        hit_record % point = r % at(root)
        hit_record % normal = ((hit_record % point) - self % center) / (self % radius)
        hit_record % mat_ptr => self % material
        call hit_record%set_face_normal(r)

        hit = .true.

    end function hit

end module mod_sphere 
