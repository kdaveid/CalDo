namespace CalDo.Models;

public class ToDoEvent
{
    public long EventId { get; set; }

    public DateTimeOffset OccurredAt { get; set; }

    public DateTime Date { get; set; }

    public int ToDoId;

    public string? Remarks { get; set; }
}
