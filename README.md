# Links

## Endpoints

The backend gives you the following endpoints:

* Create a Link

```
POST {{baseUrl}}/api/links
{"url":"http://google.com","id":1,"title":"Google"}
```

* List Links

```
GET {{baseUrl}}/api/links
[
  {
    "url": "http://google.com",
    "id": 1,
    "title": "Google"
  }
]
```

* Delete a Link

```
PUT {{baseUrl}}/api/links/1
```

* Edit a Link

```
DELETE {{baseUrl}}/api/links/1
```
