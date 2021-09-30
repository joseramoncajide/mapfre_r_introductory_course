r <- plumber::plumb(here::here("api.R"))
r$run(port = 8000)

# Tests for api methods:
# curl -X GET "http://127.0.0.1:8000/test" -H "accept: */*"
# curl -X GET "http://127.0.0.1:8000/prediction?pregnant=2&glucose=95&pressure=70&triceps=31&insulin=102&mass=28.2&pedigree=0.67&age=47" -H "accept: */*"