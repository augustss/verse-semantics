import { parseVersee } from './src/parser/parser';

const source = 'var result := 42';
console.log('Checking if pattern field exists...');
const result = parseVersee(source);

if (result.success) {
    console.log('AST:', JSON.stringify(result.value, (_k, v) => typeof v === 'bigint' ? v.toString() + 'n' : v, 2));
} else {
    console.log('Parse failed:', result.error);
}