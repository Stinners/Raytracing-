module mod_dielectric 
    use mod_hittable, only: material_t, hit_record_t
    use mod_ray, only: Ray
    use mod_vec3, only: unit_vec
    implicit none 

    type, extends(material_t) :: dielectric_t
        real(8) :: index_of_refraction 
    contains 
        procedure :: scatter 
    end type dielectric_t

contains 

    function refract(uv, n, etai_over_etai)  result(res)
        real(8), intent(in) :: uv(3), n(3)
        real(8), intent(in) :: etai_over_etai
        real(8) :: res(3)

        real(8) :: cos_theta, r_out_perp(3), r_out_parallel(3)

        cos_theta = min(dot_product(-uv, n), 1.0)
        r_out_perp = etai_over_etai * (uv + cos_theta*n)
        r_out_parallel = -sqrt(abs(1.0 - dot_product(r_out_perp, r_out_perp))) * n
        res = r_out_perp + r_out_parallel
    end function refract

    logical function scatter(self, r, record, attenuation, scattered)
        class(dielectric_t), intent(in) :: self
        class(Ray), intent(in) :: r
        class(hit_record_t), intent(in) :: record
        real(8), intent(out) :: attenuation(3)
        type(Ray), intent(out) :: scattered

        real(8), dimension(3) :: unit_direction, refracted
        real(8) :: refraction_ratio

        attenuation = [1.0, 1.0, 1.0]
        refraction_ratio = merge(1.0/self%index_of_refraction, self%index_of_refraction, record%front_face)

        unit_direction = unit_vec(r % direction)
        refracted = refract(unit_direction, record%normal, refraction_ratio)
        scattered = Ray(record%point, refracted)
        scatter = .true.
    end function scatter


end module mod_dielectric
