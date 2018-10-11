# todohs

### Running the server:

If the project path already contains a `./dev.db` file, remove it with `rm dev.db`.

Then run `stack build` and `stack exec todohs-exe` to run the server.

### Testing authentication

After server is running, run `sh test_login.sh "uravity@gmail.com" "deku"` to log in as the test user.
The HTTP response should look something like this:

```
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8081 (#0)
> POST /login HTTP/1.1
> Host: localhost:8081
> User-Agent: curl/7.47.0
> Accept: */*
> Content-Type: application/json
> Content-Length: 51
>
* upload completely sent off: 51 out of 51 bytes
< HTTP/1.1 204 No Content
< Date: Thu, 11 Oct 2018 04:45:42 GMT
< Server: Warp/3.2.25
< Content-Type: application/json;charset=utf-8
< Set-Cookie: JWT-Cookie={token here}; Path=/; HttpOnly; Secure; SameSite=Lax
< Set-Cookie: XSRF-TOKEN=2+VvLOBw3m6MDb1S+bYl13ZxDuEtwlO6anL9YTSBeDE=; Path=/; Secure
<
* Connection #0 to host localhost left intact
```

The JWT token is inside the `JWT-Cookie` field.

Then to test successfully accessing a protected route, run `sh test_user_get.sh "{token here}"` and pass in the JWT token.

This should return:
```
{"_id":0,"_email":"uravity@gmail.com"}
```

If the token is incorrect, the HTTP response should return a 401 status code.
