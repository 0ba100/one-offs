import sys
from PyQt6.QtWidgets import QApplication, QMainWindow, QLabel, QVBoxLayout, QWidget, QFileDialog, QStatusBar
from PyQt6.QtGui import QPixmap, QMouseEvent, QAction
from PyQt6.QtCore import Qt, QPoint

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
        self.current_pixmap = None # To store the currently displayed (possibly scaled) pixmap
        self.image_path = None

        # Status bar for coordinates
        self.status_bar = QStatusBar()
        self.setStatusBar(self.status_bar)
        self.coords_label = QLabel("X: -, Y: -")
        self.status_bar.addPermanentWidget(self.coords_label)

        # Layout
        central_widget = QWidget()
        layout = QVBoxLayout(central_widget)
        layout.addWidget(self.image_label)
        self.setCentralWidget(central_widget)

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
            self.original_pixmap = QPixmap(file_name)
            if self.original_pixmap.isNull():
                self.image_label.setText("Failed to load image.")
                self.original_pixmap = None
                self.current_pixmap = None
            else:
                self.display_image()

    def display_image(self):
        if self.original_pixmap:
            # Scale pixmap to fit the label while maintaining aspect ratio
            self.current_pixmap = self.original_pixmap.scaled(
                self.image_label.size(),
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation
            )
            self.image_label.setPixmap(self.current_pixmap)
            self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter) # Re-center after setting pixmap
        else:
            self.image_label.setText("Open an image to begin.")
            self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)


    def image_mouse_move(self, event: QMouseEvent):
        if self.current_pixmap and not self.current_pixmap.isNull():
            # Calculate the position of the top-left corner of the displayed pixmap within the label
            label_size = self.image_label.size()
            pixmap_size = self.current_pixmap.size()

            # If pixmap is smaller than label, it's centered.
            # Calculate the offset from the label's edge to the pixmap's edge.
            offset_x = max(0, (label_size.width() - pixmap_size.width()) // 2)
            offset_y = max(0, (label_size.height() - pixmap_size.height()) // 2)

            # Mouse position relative to the image_label widget
            widget_x = event.position().x()
            widget_y = event.position().y()

            # Mouse position relative to the top-left of the *displayed* pixmap
            pixmap_x_displayed = widget_x - offset_x
            pixmap_y_displayed = widget_y - offset_y

            # Check if the mouse is within the bounds of the displayed pixmap
            if 0 <= pixmap_x_displayed < pixmap_size.width() and \
               0 <= pixmap_y_displayed < pixmap_size.height():

                # Scale these coordinates back to the original image dimensions
                if self.original_pixmap and pixmap_size.width() > 0 and pixmap_size.height() > 0:
                    original_x = int(pixmap_x_displayed * (self.original_pixmap.width() / pixmap_size.width()))
                    original_y = int(pixmap_y_displayed * (self.original_pixmap.height() / pixmap_size.height()))
                    self.coords_label.setText(f"X: {original_x}, Y: {original_y}")
                else:
                    self.coords_label.setText("X: -, Y: - (Error scaling)")
            else:
                # Mouse is outside the displayed pixmap area (e.g., in the padding)
                self.coords_label.setText("X: -, Y: -")
        else:
            self.coords_label.setText("X: -, Y: -")

    def resizeEvent(self, event):
        # Override resizeEvent to re-scale and display the image when window (and thus label) is resized
        super().resizeEvent(event)
        if self.original_pixmap: # Only if an image is loaded
            self.display_image()


if __name__ == "__main__":
    app = QApplication(sys.argv)
    main_window = ImageLociPainter()
    sys.exit(app.exec())
