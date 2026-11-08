// The directory's verification mail (GDS's implementation pragma Listing:
// "the directory verifies an email address by sending it a message with a
// link, which Alice opens"), sent through an SMTP relay given by
// configuration.
import 'dart:io';

import 'package:mailer/mailer.dart' as m;
import 'package:mailer/smtp_server.dart';

abstract class Mailer {
  Future<void> send(String to, String subject, String body);
}

/// The relay: its host and port, whether it speaks TLS from the start (465)
/// or upgrades (587), the account and the file holding its password, kept
/// outside the repository, and the sender address.
class SmtpConfig {
  final String host, from;
  final int port;
  final bool ssl;
  final String? user, passwordFile;
  SmtpConfig({required this.host, required this.port, required this.from, this.ssl = false, this.user, this.passwordFile});

  /// From the environment: SMTP_HOST, SMTP_PORT (587), SMTP_SSL (false),
  /// SMTP_USER, SMTP_PASSWORD_FILE and MAIL_FROM; null where SMTP_HOST is
  /// not set.
  static SmtpConfig? fromEnvironment([Map<String, String>? env]) {
    final e = env ?? Platform.environment;
    final host = e['SMTP_HOST'];
    if (host == null || host.isEmpty) return null;
    return SmtpConfig(
      host: host,
      port: int.tryParse(e['SMTP_PORT'] ?? '') ?? 587,
      ssl: (e['SMTP_SSL'] ?? 'false') == 'true',
      user: e['SMTP_USER'],
      passwordFile: e['SMTP_PASSWORD_FILE'],
      from: e['MAIL_FROM'] ?? 'directory@peoplesweb.org',
    );
  }
}

class SmtpMailer implements Mailer {
  final SmtpConfig c;
  SmtpMailer(this.c);
  @override
  Future<void> send(String to, String subject, String body) async {
    final pw = c.passwordFile == null ? null : File(c.passwordFile!).readAsStringSync().trim();
    final server = SmtpServer(c.host, port: c.port, ssl: c.ssl, username: c.user, password: pw,
        allowInsecure: false, ignoreBadCertificate: false);
    final msg = m.Message()
      ..from = m.Address(c.from, 'Grassroots Directory')
      ..recipients.add(to)
      ..subject = subject
      ..text = body;
    await m.send(msg, server);
  }
}

/// Where no relay is configured: the mail is written to the log, for a test
/// or a development run, and nothing is sent.
class LogMailer implements Mailer {
  final List<(String, String, String)> sent = [];
  @override
  Future<void> send(String to, String subject, String body) async {
    sent.add((to, subject, body));
    stdout.writeln('[mail, not sent: no SMTP_HOST] to $to: $subject\n$body');
  }
}
