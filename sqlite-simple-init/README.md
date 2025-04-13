# sqlite-simple-init

## How to run application

```bash
rm users.db
stack run
```

## REST API Usage

The server runs on `http://localhost:3000` and provides the following endpoints:

### List all users

```
GET /users

Response: 200 OK
[{
  "userId": 1,
  "userName": "John Doe",
  "userEmail": "john@example.com",
  "userPassword": "password123",
  "createdAt": "2025-04-13T04:01:13Z",
  "updatedAt": "2025-04-13T04:01:13Z"
}]
```

### Get user by ID

```
GET /users/:id

Response: 200 OK
{
  "userId": 1,
  "userName": "John Doe",
  "userEmail": "john@example.com",
  "userPassword": "password123",
  "createdAt": "2025-04-13T04:01:13Z",
  "updatedAt": "2025-04-13T04:01:13Z"
}

Response: 404 Not Found (if user not found)
```

### Create new user

```
POST /users
Content-Type: application/x-www-form-urlencoded

Parameters:
- name: User's name
- email: User's email (must be unique)
- password: User's password

Response: 201 Created
{
  "userId": 1,
  "userName": "John Doe",
  "userEmail": "john@example.com",
  "userPassword": "password123",
  "createdAt": "2025-04-13T04:01:13Z",
  "updatedAt": "2025-04-13T04:01:13Z"
}
```

### Update user

```
PUT /users/:id
Content-Type: application/x-www-form-urlencoded

Parameters:
- name: New name
- email: New email
- password: New password

Response: 200 OK (if updated)
Response: 404 Not Found (if user not found)
```

### Delete user

```
DELETE /users/:id

Response: 204 No Content (if deleted)
Response: 404 Not Found (if user not found)
```

### Example using curl

```bash
# Create a new user
curl -X POST http://localhost:3000/users \
     -d "name=John Doe" \
     -d "email=john@example.com" \
     -d "password=password123"

# Get all users
curl http://localhost:3000/users

# Get user by ID
curl http://localhost:3000/users/1

# Update user
curl -X PUT http://localhost:3000/users/1 \
     -d "name=John Updated" \
     -d "email=john.updated@example.com" \
     -d "password=newpassword123"

# Delete user
curl -X DELETE http://localhost:3000/users/1
