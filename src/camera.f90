module mod_camera 
    use mod_ray, only: Ray
    use mod_vec3, only: unit_vec, cross_product, random_in_unit_disc
    implicit none 

    real(8), parameter :: PI = 3.1415927

    private 
    public Camera_t
    public init_camera
    public get_ray

    type :: Camera_t
        real(8), dimension(3) :: origin, horizontal, vertical, lower_left_corner, u, v
        real(8) lens_radius
    contains 
        procedure :: get_ray
    end type Camera_t

contains 

    real(8) pure function degrees_to_radians(degrees)
        real(8), intent(in) :: degrees
        degrees_to_radians = degrees * pi / 180.0
    end function degrees_to_radians


    pure function init_camera(lookfrom, lookat, vup, vfov, aspect_ratio, aperture, focus_dist) result(new)
        real(8), dimension(3), intent(in) :: lookfrom, lookat, vup
        real(8), intent(in) :: vfov, aspect_ratio, aperture, focus_dist
        type(Camera_t) :: new

        real(8) :: theta, h, viewport_height, viewport_width, focal_length
        real(8), dimension(3) :: w, u, v 

        focal_length = 1.0

        theta = degrees_to_radians(vfov)
        h = tan(theta/2)
        viewport_height = 2.0 * h
        viewport_width = aspect_ratio * viewport_height

        w = unit_vec(lookfrom - lookat)
        new % u = unit_vec(cross_product(vup, w))
        new % v = cross_product(w, new%u)
        
        new % origin  = lookfrom
        new % horizontal = focus_dist * viewport_width * new%u
        new % vertical = focus_dist * viewport_height * new%v
        new % lower_left_corner = new%origin - (new%horizontal)/2 - (new%vertical)/2 - focus_dist * w
        new % lens_radius = aperture / 2 
    end function init_camera

    function get_ray(self, s, t) result(new_ray)
        class(Camera_t), intent(in) :: self
        real(8), intent(in) :: s, t
        type(Ray) :: new_ray 
        real(8), dimension(3) :: rd, offset 

        rd = self%lens_radius * random_in_unit_disc()
        offset = self%u * rd(1) + self%v * rd(2)

        new_ray % origin = self % origin + offset
        new_ray % direction = self%lower_left_corner + s*self%horizontal + t*self%vertical - self%origin - offset
    end function get_ray

end module mod_camera 
