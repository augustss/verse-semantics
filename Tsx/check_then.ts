import { parseVersee } from './src/parser/parser';
import { PrettyPrinter } from './src/printer/pretty-printer';

const line = "if 1 then 2 else 3";
const result = parseVersee(line);

if (result.success) {
  const exp = result.value.value as any;
  const printer = new PrettyPrinter(undefined, line);
  const thenStr = printer.print(exp.then);
  
  console.log('thenStr:', JSON.stringify(thenStr));
  console.log('thenStr length:', thenStr.length);
}
