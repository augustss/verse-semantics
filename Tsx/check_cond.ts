import { parseVersee } from './src/parser/parser';
import { PrettyPrinter } from './src/printer/pretty-printer';

const line = "if 1 then 2 else 3";
const result = parseVersee(line);

if (result.success) {
  const exp = result.value.value as any;
  const printer = new PrettyPrinter(undefined, line);
  const condStr = printer.print(exp.cond);
  
  console.log('condStr:', JSON.stringify(condStr));
  console.log('condStr length:', condStr.length);
}
