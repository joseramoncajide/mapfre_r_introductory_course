---
editor_options: 
  markdown: 
    wrap: 72
---

## Github

1.  Crear una cuenta en [Github.com](https://github.com/)
2.  Crear un repositorio de prueba
3.  Crear un token de acceso

**Instrucciones para crear un token de acceso:**

> <https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token>
>
> <https://docs.github.com/en/get-started/getting-started-with-git/caching-your-github-credentials-in-git>

### Create a new repository on the command line

`echo "# mapfre_r_introductory_course" >> README.md`

`git init git add README.md`

`git commit -m "first commit"`

`git branch -M main`

`git remote add origin https://github.com/joseramoncajide/mapfre_r_introductory_course.git`

`git push -u origin main`

### Push an existing repository from the command line

`git remote add origin https://github.com/joseramoncajide/mapfre_r_introductory_course.git`

`git branch -M main git push -u origin main`

#### Para forzar que pida la contraseña en caso de cambio de token repetir:

`git remote add origin https://github.com/joseramoncajide/mapfre_r_introductory_course.git`
