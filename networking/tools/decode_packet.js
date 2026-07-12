#!/usr/bin/env node

// Decode BitchatPacket from byte array
//
// Usage: node decode_packet.js '[6, 7, 105, ...]'
// Or paste the array when prompted

const PACKET_TYPES = {
  0x01: 'ANNOUNCE',
  0x02: 'MESSAGE',
  0x03: 'FRAGMENT_START',
  0x04: 'FRAGMENT_CONTINUE',
  0x05: 'FRAGMENT_END',
  0x06: 'ACK',
  0x07: 'NACK',
  0x08: 'READ_RECEIPT',
};

const HEADER_SIZE = 152;

function toHex(bytes) {
  return Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
}

function bytesToUuid(bytes) {
  const hex = toHex(bytes);
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20, 32)}`;
}

function readUint16BE(data, offset) {
  return (data[offset] << 8) | data[offset + 1];
}

function readUint32BE(data, offset) {
  return ((data[offset] << 24) | (data[offset + 1] << 16) | (data[offset + 2] << 8) | data[offset + 3]) >>> 0;
}

function isAllZeros(bytes) {
  return bytes.every(b => b === 0);
}

function decodeAnnouncePayload(payload) {
  // Format: [pubkey(32) + version(2) + nickLen(1) + nick + addrLen(2) + addr?]
  let offset = 0;

  const pubkey = payload.slice(offset, offset + 32);
  offset += 32;

  const version = readUint16BE(payload, offset);
  offset += 2;

  const nickLen = payload[offset];
  offset += 1;

  const nickname = new TextDecoder().decode(new Uint8Array(payload.slice(offset, offset + nickLen)));
  offset += nickLen;

  let address = null;
  if (offset + 2 <= payload.length) {
    const addrLen = readUint16BE(payload, offset);
    offset += 2;
    if (addrLen > 0 && offset + addrLen <= payload.length) {
      address = new TextDecoder().decode(new Uint8Array(payload.slice(offset, offset + addrLen)));
    }
  }

  console.log('  --- ANNOUNCE Payload ---');
  console.log(`  Pubkey:    ${toHex(pubkey)}`);
  console.log(`  Version:   ${version}`);
  console.log(`  Nickname:  "${nickname}" (${nickLen} bytes)`);
  if (address) {
    console.log(`  Address:   ${address}`);
  } else {
    console.log(`  Address:   (none)`);
  }
}

function decodePacket(bytes) {
  if (bytes.length < HEADER_SIZE) {
    console.error(`Packet too small: ${bytes.length} < ${HEADER_SIZE}`);
    console.log('\nRaw bytes as UTF-8:', new TextDecoder().decode(new Uint8Array(bytes)));
    return;
  }

  let offset = 0;

  // Type (1 byte)
  const typeValue = bytes[offset++];
  const typeName = PACKET_TYPES[typeValue] || `UNKNOWN(0x${typeValue.toString(16)})`;

  // TTL (1 byte)
  const ttl = bytes[offset++];

  // Timestamp (4 bytes, big-endian)
  const timestamp = readUint32BE(bytes, offset);
  offset += 4;
  const date = new Date(timestamp * 1000);

  // Sender pubkey (32 bytes)
  const senderPubkey = bytes.slice(offset, offset + 32);
  offset += 32;

  // Recipient pubkey (32 bytes)
  const recipientPubkey = bytes.slice(offset, offset + 32);
  const isBroadcast = isAllZeros(recipientPubkey);
  offset += 32;

  // Payload length (2 bytes, big-endian)
  const payloadLength = readUint16BE(bytes, offset);
  offset += 2;

  // Packet ID (16 bytes UUID)
  const packetIdBytes = bytes.slice(offset, offset + 16);
  const packetId = bytesToUuid(packetIdBytes);
  offset += 16;

  // Signature (64 bytes)
  const signature = bytes.slice(offset, offset + 64);
  offset += 64;

  // Payload
  const payload = bytes.slice(offset, offset + payloadLength);

  console.log('=== BitchatPacket ===');
  console.log(`Type:        ${typeName} (0x${typeValue.toString(16).padStart(2, '0')})`);
  console.log(`TTL:         ${ttl}`);
  console.log(`Timestamp:   ${timestamp} (${date.toISOString()})`);
  console.log(`Sender:      ${toHex(senderPubkey)}`);
  console.log(`Recipient:   ${isBroadcast ? '(broadcast)' : toHex(recipientPubkey)}`);
  console.log(`Payload len: ${payloadLength}`);
  console.log(`Packet ID:   ${packetId}`);
  console.log(`Signature:   ${toHex(signature.slice(0, 8))}...`);
  console.log(`Total bytes: ${bytes.length} (header: ${HEADER_SIZE}, payload: ${payloadLength})`);

  if (payload.length > 0) {
    console.log('');

    if (typeName === 'ANNOUNCE') {
      decodeAnnouncePayload(payload);
    } else if (typeName === 'ACK' || typeName === 'READ_RECEIPT') {
      const text = new TextDecoder().decode(new Uint8Array(payload));
      console.log(`  --- ${typeName} Payload ---`);
      console.log(`  Message ID: ${text}`);
    } else if (typeName === 'MESSAGE') {
      const text = new TextDecoder().decode(new Uint8Array(payload));
      const isPrintable = /^[\x20-\x7e\n\r\t]+$/.test(text);
      console.log('  --- MESSAGE Payload ---');
      if (isPrintable) {
        console.log(`  Text: ${text}`);
      } else {
        console.log(`  Hex:  ${toHex(payload)}`);
        console.log(`  Raw:  [${payload.join(', ')}]`);
      }
    } else {
      console.log(`  --- Payload (raw) ---`);
      console.log(`  Hex:  ${toHex(payload)}`);
      console.log(`  Raw:  [${payload.join(', ')}]`);
    }
  }
}

// --- Main ---

function parseInput(input) {
  // Strip surrounding brackets and whitespace
  input = input.trim().replace(/^\[/, '').replace(/\]$/, '');
  return input.split(',').map(s => parseInt(s.trim(), 10)).filter(n => !isNaN(n));
}

const arg = process.argv.slice(2).join(' ');
if (arg) {
  decodePacket(parseInput(arg));
} else {
  // Read from stdin
  let data = '';
  process.stdin.setEncoding('utf8');
  process.stdin.on('data', chunk => data += chunk);
  process.stdin.on('end', () => {
    if (data.trim()) {
      decodePacket(parseInput(data));
    } else {
      console.log('Usage: node decode_packet.js \'[6, 7, 105, ...]\'');
      console.log('   or: echo \'[6, 7, ...]\' | node decode_packet.js');
    }
  });
}
