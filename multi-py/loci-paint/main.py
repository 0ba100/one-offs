import sys
from PyQt6.QtWidgets import QApplication, QMainWindow, QLabel, QWidget, QFileDialog, QStatusBar, QScrollArea
from PyQt6.QtGui import QPixmap, QMouseEvent, QAction
from PyQt6.QtCore import Qt

class ImageLociPainter(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("LociPaint - Image Coordinate Viewer")
        self.setGeometry(100, 100, 800, 600)

        self.image_label = QLabel("Open an image to begin.")
        self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.image_label.setMouseTracking(True) # Enable mouse tracking on the label
        self.image_label.mouseMoveEvent = self.image_mouse_move

        self.original_pixmap = None # To store the original loaded pixmap
        self.image_path = None

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

        self.show()

    def open_image(self):
        file_name, _ = QFileDialog.getOpenFileName(self, "Open Image", "", "Image Files (*.png *.jpg *.jpeg *.bmp *.gif)")
        if file_name:
            self.image_path = file_name
            pixmap = QPixmap(file_name)
            if pixmap.isNull():
                self.original_pixmap = None
                self.image_label.setText("Failed to load image.")
                self.image_label.setPixmap(QPixmap()) # Clear image
                self.image_label.adjustSize()
            else:
                self.original_pixmap = pixmap
                self.display_image()

    def display_image(self):
        if self.original_pixmap:
            self.image_label.setPixmap(self.original_pixmap)
            self.image_label.adjustSize() # Resize label to pixmap


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
