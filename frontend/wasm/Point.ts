@unmanaged
export default class Point {
  x: f32 = 0;
  y: f32 = 0;

  constructor(x: f32 = 0, y: f32 = 0) {
    this.x = x;
    this.y = y;
  }

  get length(): f32 {
    return Math.hypot(this.x, this.y)
  }

  get lengthSquared(): f32 {
    return this.x * this.x + this.y * this.y
  }

  @inline
  set(x: f32, y: f32): Point {
    this.x = x;
    this.y = y;
    return this;
  }

  copy(other: Point): Point {
    return this.set(other.x, other.y)
  }

  clone(): Point {
    return new Point(this.x, this.y)
  }

  add(other: Point): Point {
    return this.set(this.x + other.x, this.y + other.y);
  }

  addScaled(other: Point, scale: f32): Point {
    return this.set(this.x + other.x * scale, this.y + other.y * scale);
  }

  subtract(other: Point): Point {
    return this.set(this.x - other.x, this.y - other.y);
  }

  multiply(other: Point): Point {
    return this.set(this.x * other.x, this.y * other.y);
  }

  divide(other: Point): Point {
    return this.set(this.x / other.x, this.y / other.y);
  }

  normalize(): Point {
    const l: f32 = this.length
    return this.set(this.x / l, this.y / l)
  }

  negate(): Point {
    return this.set(-this.x, -this.y)
  }

  perpLeft(): Point {
    return this.set(this.y, -this.x)
  }

  perpRight(): Point {
    return this.set(-this.y, this.x)
  }

  dot(other: Point): f32 {
    return this.x * other.x + this.y * other.y
  }

  cross(other: Point): f32 {
    return this.x * other.y - this.y * other.x
  }

  rotate(angle: f32): Point {
    const c: f32 = Math.cos(angle)
    const s: f32 = Math.sin(angle)
    return this.set(
      c * this.x - s * this.y,
      s * this.x + c * this.y
    )
  }

  scale(scale: f32): Point {
    return this.set(this.x * scale, this.y * scale)
  }
}
