import Rect from './Rect'

export const selRect: Rect = new Rect()
export const value: i32 = 666

/**
 *
 */
export function computeDerived(): void
{
  selRect.position.set(5, 4)
  selRect.size.set(25, 4)
}
