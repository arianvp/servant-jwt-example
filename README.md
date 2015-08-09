# servant-jwt-example
Servant authentication with javascript web tokens

## **DISCLAIMER**
This is by no means production ready and I would highly advice to NOT use this in production.
The reasons are that currently the `jwt` package used is very incomplete and only verifies the signature of the jwt token and not actually the content.   This means people can use expired tokens to reach your API. Which is bad.

I'm in the process of porting the thing to `jose`  to support more secure defaults

## Usage

To request a token.  One has to do a HTTP Basic-Auth GET request  to  /grant.   which will return a JWT token for that user in the following format:
```
$ curl -i -u arian:test http://localhost:8080/grant 
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sat, 08 Aug 2015 22:55:11 GMT
Server: Warp/3.0.13.1
Content-Type: application/json

{"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlcyI6WyJIb21lcGFnZSIsIk1zZyJdfQ.xHlad8XHhxLBZLXBL1irPXBoP8LQBOn3q8Ec7NS5hbk"}%
```

Now that you have a token. you can make requests to the API

```
$curl -i http://localhost:8080/message -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlcyI6WyJIb21lcGFnZSIsIk1zZyJdfQ.xHlad8XHhxLBZLXBL1irPXBoP8LQBOn3q8Ec7NS5hbk"
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sat, 08 Aug 2015 22:56:55 GMT
Server: Warp/3.0.13.1
Content-Type: application/json

{"message":"Welcome. you're authorized"}
```

An invalid token leads to a 401 unauthorized.

If a user has (for example peter) uses his token to request a resource his role doesn't allow. a 403 forbidden is thrown:

```
$ curl -i http://localhost:8080/message -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlcyI6W119.LxYclrQN6BbjAsRWWbK4buwJwUsHCG9OFaYhkmXSu8w"          
HTTP/1.1 403 Forbidden                                                                                                                                               
Transfer-Encoding: chunked
Date: Sat, 08 Aug 2015 22:58:36 GMT
Server: Warp/3.0.13.1
```
