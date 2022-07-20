namespace CalDo.Models;

public class ToDoEvent
{
    public int EventId { get; set; }

    public DateTime Date { get; set; }

    public string CalendarToDoId { get; set; } = string.Empty;

    public string? Remarks { get; set; }

    public bool? AdjustCalendar { get; set; }
}
