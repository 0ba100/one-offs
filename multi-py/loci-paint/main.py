import sys
from PyQt6.QtWidgets import QApplication, QMainWindow, QLabel, QWidget, QFileDialog, QStatusBar, QScrollArea, QSlider
from PyQt6.QtGui import QPixmap, QMouseEvent, QAction, QPainter, QPen, QBrush, QKeySequence
from PyQt6.QtCore import Qt, QRect

class ImageLociPainter(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("LociPaint - Image Coordinate Viewer")
        self.setGeometry(100, 100, 800, 600)

        self.image_label = QLabel("Open an image to begin.")
        self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.image_label.setMouseTracking(True) # Enable mouse tracking on the label
        self.image_label.mouseMoveEvent = self.image_mouse_move
        self.image_label.mousePressEvent = self.image_mouse_press

        self.original_pixmap = None # To store the original loaded pixmap
        self.image_path = None
        self.points = []
        self.point_radius = 10

        # Status bar for coordinates
        self.status_bar = QStatusBar()
        self.setStatusBar(self.status_bar)
        self.coords_label = QLabel("X: -, Y: -")
        self.status_bar.addPermanentWidget(self.coords_label)

        # Layout
        self.scroll_area = QScrollArea()
        self.scroll_area.setWidget(self.image_label)
        self.scroll_area.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setCentralWidget(self.scroll_area)

        # Menu bar
        menubar = self.menuBar()
        file_menu = menubar.addMenu("&File")

        open_action = QAction("&Open Image...", self)
        open_action.triggered.connect(self.open_image)
        file_menu.addAction(open_action)

        exit_action = QAction("&Exit", self)
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)

        # Edit menu for undo
        edit_menu = menubar.addMenu("&Edit")
        undo_action = QAction("&Undo", self)
        undo_action.setShortcut(QKeySequence.StandardKey.Undo)
        undo_action.triggered.connect(self.undo_last_point)
        edit_menu.addAction(undo_action)

        # Toolbar for point size
        toolbar = self.addToolBar("Point Size")
        toolbar.addWidget(QLabel("Point Size:"))

        self.radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.radius_slider.setRange(2, 50)
        self.radius_slider.setValue(self.point_radius)
        self.radius_slider.valueChanged.connect(self.set_radius)
        toolbar.addWidget(self.radius_slider)

        self.radius_label = QLabel(f" {self.point_radius}px")
        toolbar.addWidget(self.radius_label)

        self.show()

    def set_radius(self, value):
        self.point_radius = value
        self.radius_label.setText(f" {value}px")
        self.update_image_with_drawings()

    def open_image(self):
        file_name, _ = QFileDialog.getOpenFileName(self, "Open Image", "", "Image Files (*.png *.jpg *.jpeg *.bmp *.gif)")
        if file_name:
            self.image_path = file_name
            pixmap = QPixmap(file_name)
            if pixmap.isNull():
                self.original_pixmap = None
                self.points = []
                self.image_label.setText("Failed to load image.")
                self.image_label.setPixmap(QPixmap()) # Clear image
                self.image_label.adjustSize()
            else:
                self.original_pixmap = pixmap
                self.points = []
                self.display_image()

    def display_image(self):
        if self.original_pixmap:
            self.update_image_with_drawings()
            self.image_label.adjustSize() # Resize label to pixmap

    def update_image_with_drawings(self):
        if not self.original_pixmap:
            return

        drawn_pixmap = self.original_pixmap.copy()
        painter = QPainter(drawn_pixmap)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Draw line connecting points
        if len(self.points) > 1:
            pen = QPen(Qt.GlobalColor.red, 2, Qt.PenStyle.SolidLine)
            painter.setPen(pen)
            painter.drawPolyline(self.points)

        # Draw circles with numbers
        radius = self.point_radius
        font = painter.font()
        font.setBold(True)
        font.setPointSize(self.point_radius)
        painter.setFont(font)

        for i, point in enumerate(self.points):
            # Draw circle
            painter.setPen(QPen(Qt.GlobalColor.red, 2))
            painter.setBrush(QBrush(Qt.GlobalColor.yellow))
            painter.drawEllipse(point, radius, radius)

            # Draw number
            painter.setPen(QPen(Qt.GlobalColor.black))
            text_rect = QRect(int(point.x()) - radius, int(point.y()) - radius, 2 * radius, 2 * radius)
            painter.drawText(text_rect, Qt.AlignmentFlag.AlignCenter, str(i + 1))

        painter.end()
        self.image_label.setPixmap(drawn_pixmap)

    def image_mouse_press(self, event: QMouseEvent):
        if event.button() == Qt.MouseButton.LeftButton and self.original_pixmap and not self.original_pixmap.isNull():
            pos = event.position()

            # Check if the mouse is within the bounds of the pixmap
            if 0 <= pos.x() < self.original_pixmap.width() and 0 <= pos.y() < self.original_pixmap.height():
                self.points.append(pos)
                self.update_image_with_drawings()

    def undo_last_point(self):
        if self.points:
            self.points.pop()
            self.update_image_with_drawings()

    def image_mouse_move(self, event: QMouseEvent):
        if self.original_pixmap and not self.original_pixmap.isNull():
            # The position on the label is the position on the image
            pos = event.position()
            x = int(pos.x())
            y = int(pos.y())

            # Check if the mouse is within the bounds of the pixmap
            if 0 <= x < self.original_pixmap.width() and 0 <= y < self.original_pixmap.height():
                self.coords_label.setText(f"X: {x}, Y: {y}")
            else:
                # Mouse is outside the displayed pixmap area
                self.coords_label.setText("X: -, Y: -")
        else:
            self.coords_label.setText("X: -, Y: -")


if __name__ == "__main__":
    app = QApplication(sys.argv)
    main_window = ImageLociPainter()
    sys.exit(app.exec())
