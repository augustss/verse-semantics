# Grouping

Group your Verse expressions to specify order of evaluation and improve readability.
Grouping expressions is a way to specify order of evaluation, which is useful if you need to work around operator precedence.

You can group expressions by using ().

For example, the expressions (y2 - y1) and (x2 - x1) below are evaluated before dividing the numbers.

(y2 - y1) / (x2 - x1)

As an example, take an in-game explosion that scales its damage based on the distance from the player, but where the player's armor can reduce the total damage:

```verse
BaseDamage : float = 100
Armor : float = 15