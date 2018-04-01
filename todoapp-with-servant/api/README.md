## Todo API documentation

This is a simple REST API in Servant for the even simpler Todo App.

Elm query functions and API documentations are generated (servant-elm, servant-docs)

API version: 1.0.0

## GET /todo

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"done":false,"value":"buy me a beer","id":0}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"done":false,"value":"buy me a beer","id":0},{"done":false,"value":"buy me a beer","id":0}]
    ```

## POST /todo

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"value":"buy me a beer"}
    ```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"done":false,"value":"buy me a beer","id":0}
    ```

## DELETE /todo/:todoId

### Captures:

- *todoId*: (integer) Todo ID

### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

## GET /todo/:todoId

### Captures:

- *todoId*: (integer) Todo ID

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
null
    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"done":false,"value":"buy me a beer","id":0}
    ```

## PUT /todo/:todoId

### Captures:

- *todoId*: (integer) Todo ID

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"done":false,"value":"buy me a beer","id":0}
    ```

### Response:

- Status code 202
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

