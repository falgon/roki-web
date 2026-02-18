#!/usr/bin/env python3
"""OGP画像生成スクリプト"""
from __future__ import annotations

from pathlib import Path
import math

from PIL import Image, ImageDraw, ImageFont

WIDTH = 1200
HEIGHT = 630
FONT_CANDIDATES = (
    "Arial.ttf",
    "Helvetica.ttf",
    "/System/Library/Fonts/Supplemental/Arial.ttf",
    "/System/Library/Fonts/Supplemental/Helvetica.ttf",
    "/System/Library/Fonts/Helvetica.ttc",
    "/Library/Fonts/Arial.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
)


def hex_to_rgb(hex_color: str) -> tuple[int, int, int]:
    """16進数カラーコードをRGBタプルに変換"""
    hex_value = hex_color.lstrip("#")
    if len(hex_value) != 6:
        raise ValueError(f"Invalid hex color: {hex_color}")
    return tuple(int(hex_value[i : i + 2], 16) for i in range(0, 6, 2))


def create_gradient_image(
    width: int,
    height: int,
    color1: tuple[int, int, int],
    color2: tuple[int, int, int],
    direction: str = "horizontal",
) -> Image.Image:
    """グラデーション背景画像を生成

    direction: "horizontal", "vertical", "diagonal"
    """
    image = Image.new("RGB", (width, height))
    if direction == "horizontal":
        draw = ImageDraw.Draw(image)
        for x in range(width):
            ratio = x / (width - 1) if width > 1 else 0
            color = tuple(
                int(color1[i] + (color2[i] - color1[i]) * ratio) for i in range(3)
            )
            draw.line([(x, 0), (x, height)], fill=color)
        return image
    if direction == "vertical":
        draw = ImageDraw.Draw(image)
        for y in range(height):
            ratio = y / (height - 1) if height > 1 else 0
            color = tuple(
                int(color1[i] + (color2[i] - color1[i]) * ratio) for i in range(3)
            )
            draw.line([(0, y), (width, y)], fill=color)
        return image
    if direction == "diagonal":
        pixels = image.load()
        denom = (width + height - 2) or 1
        for y in range(height):
            for x in range(width):
                ratio = (x + y) / denom
                color = tuple(
                    int(color1[i] + (color2[i] - color1[i]) * ratio)
                    for i in range(3)
                )
                pixels[x, y] = color
        return image
    raise ValueError(f"Invalid direction: {direction}")


def get_font(size: int) -> ImageFont.FreeTypeFont:
    """利用可能なシステムフォントを取得（フォールバック付き）"""
    for font_path in FONT_CANDIDATES:
        try:
            return ImageFont.truetype(font_path, size=size)
        except OSError:
            continue
    return ImageFont.load_default()


def create_ogp_image(
    filename: str,
    main_text: str,
    sub_text: str | None,
    bg_color1: str,
    bg_color2: str,
    text_color: str,
    sub_text_color: str | None,
    main_font_size: int,
    sub_font_size: int,
    gradient_direction: str,
    output_dir: Path,
) -> None:
    """OGP画像を生成して保存"""
    background = create_gradient_image(
        WIDTH,
        HEIGHT,
        hex_to_rgb(bg_color1),
        hex_to_rgb(bg_color2),
        direction=gradient_direction,
    )
    draw = ImageDraw.Draw(background)
    main_font = get_font(main_font_size)
    main_bbox = draw.textbbox((0, 0), main_text, font=main_font)
    main_width = main_bbox[2] - main_bbox[0]
    main_height = main_bbox[3] - main_bbox[1]

    if sub_text is not None:
        sub_font = get_font(sub_font_size)
        sub_bbox = draw.textbbox((0, 0), sub_text, font=sub_font)
        sub_width = sub_bbox[2] - sub_bbox[0]
        sub_height = sub_bbox[3] - sub_bbox[1]
        line_gap = int(math.floor(main_font_size * 0.2))
        total_height = main_height + line_gap + sub_height
        start_y = (HEIGHT - total_height) / 2
        main_x = (WIDTH - main_width) / 2
        sub_x = (WIDTH - sub_width) / 2
        draw.text(
            (main_x, start_y),
            main_text,
            fill=hex_to_rgb(text_color),
            font=main_font,
        )
        draw.text(
            (sub_x, start_y + main_height + line_gap),
            sub_text,
            fill=hex_to_rgb(sub_text_color or text_color),
            font=sub_font,
        )
    else:
        main_x = (WIDTH - main_width) / 2
        main_y = (HEIGHT - main_height) / 2
        draw.text(
            (main_x, main_y),
            main_text,
            fill=hex_to_rgb(text_color),
            font=main_font,
        )

    output_path = output_dir / filename
    background.save(output_path, format="PNG", optimize=True)


def main() -> None:
    """メイン処理"""
    output_dir = Path("contents/images/ogp")
    output_dir.mkdir(parents=True, exist_ok=True)

    create_ogp_image(
        filename="default.png",
        main_text="roki.dev",
        sub_text=None,
        bg_color1="#1a1a2e",
        bg_color2="#16213e",
        text_color="#ffffff",
        sub_text_color=None,
        main_font_size=120,
        sub_font_size=0,
        gradient_direction="diagonal",
        output_dir=output_dir,
    )
    create_ogp_image(
        filename="roki-log-default.png",
        main_text="roki.log",
        sub_text="Technology & Learning",
        bg_color1="#0f3460",
        bg_color2="#16697a",
        text_color="#ffffff",
        sub_text_color="#94bbe9",
        main_font_size=100,
        sub_font_size=32,
        gradient_direction="horizontal",
        output_dir=output_dir,
    )
    create_ogp_image(
        filename="roki-diary-default.png",
        main_text="roki.diary",
        sub_text="Daily Thoughts",
        bg_color1="#f5e6d3",
        bg_color2="#f9d5d3",
        text_color="#3d2c2e",
        sub_text_color="#6d5c5e",
        main_font_size=100,
        sub_font_size=32,
        gradient_direction="horizontal",
        output_dir=output_dir,
    )


if __name__ == "__main__":
    main()
