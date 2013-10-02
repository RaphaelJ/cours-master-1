package model;

public class Row {

	private int _width;
	private Cell[] _cells;

	public Row(int width) {
		
		_width = width;
		_cells = new Cell[width];
		
		for(int i = 0; i < width; i++)
			_cells[i] = new Cell(false);
	}
	
	public Cell[] getCells() {
		return _cells;
	}
	
	public Cell getCell(int column) throws Exception {
		if(column < 0 || column >= _width)
			throw new Exception("Column out of range.");
		
		return _cells[column];
	}
	
	public void reset() {
		
		for(Cell cell : _cells)
			cell.setBlock(false);
	}
}
