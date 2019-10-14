from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QVBoxLayout
app = QApplication([])
window = QWidget()
layout = QVBoxLayout()
top = QPushButton('Top')
bottom = QPushButton('Bottom')
layout.addWidget(top)

def on_button_clicked():
    exit(0);
top.clicked.connect(on_button_clicked)
bottom.clicked.connect(on_button_clicked)

layout.addWidget(bottom)
window.setLayout(layout)
window.show()
app.exec()
