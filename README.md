# Сервис отправки электронных сообщений

Позволяет отправлять электронные сообщения (email) любой сложности.

## Запуск сервиса

```
docker run -d --name=send-email <image-name:tag>
```

Для локального использования необходимо пробросить порт 7777 на хост:

```
docker run -d --name=send-email -p <port>:7777 <image-name:tag>
```

## Отправка сообщения

Для отправки сообщения необходимо выполнить `multipart/form-data` POST-запрос, каждая часть тела которого содержит составные части электронного сообщения:

- заголовки сообщения (Content-Type: application/json)

```
    {
        "from": "Sender Name<from@email.com>",
        "to": ["RCPT1<rcpt1@email.com>", "RCPT2<rcpt2@email.com>, ...],
        "cc": [...],
        "bcc": [...],
        "subject": "Email subject"
    }
```

Обязательными являются поля `from`, `to` и `subject`

- текстовый вариант письма (Content-Type: text/plain)
- html-вариант письма (Content-Type: text/html)
- вложения (тип вложения соответствует типу отправляемого файла)

## Пример кода отправки на языке Dart

```dart
  final mpr = http.MultipartRequest('POST', Uri.parse('http://localhost:7777'))
    ..files.addAll([
      http.MultipartFile.fromString(
          'headers',
          json.encode({
            'from': 'AFS Notification<no-reply@allfuneral.com>',
            'to': ['Recipient Name<user@email.com>'],
            // 'cc': [],
            // 'bcc': ['Blind Copy<bcc@email.com>'],
            'subject': 'Notfication'
          }),
          filename: 'headers.json',
          contentType: MediaType('application', 'json')),
      http.MultipartFile.fromString(
          'plain',
          '''
      Hi!

      This is the plain text message for you with attachment
      ''',
          filename: 'message.txt',
          contentType: MediaType('text', 'plain')),
      http.MultipartFile.fromString(
          'html',
          '''
      <html>
        <head>
          <title>Message</title>
        </head>
        <body>
          <h2>Hi!</h2>
          <p>This is the HTML message for you with attachment</p>
        </body>
      </html>
      ''',
          filename: 'message.html',
          contentType: MediaType('text', 'html')),
      await http.MultipartFile.fromPath('attachment', 'README.md')
    ]);
  final response = await http.Response.fromStream(await mpr.send());
  print(response.reasonPhrase);
```
