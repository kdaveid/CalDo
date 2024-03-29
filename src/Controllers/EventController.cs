using CalDo.Functions;
using CalDo.Models;
using Ical.Net.CalendarComponents;
using Ical.Net.DataTypes;
using Microsoft.AspNetCore.Mvc;

namespace CalDo.Controllers;

[ApiController]
[Route("api/events")]
public class EventController : ControllerBase
{
    private readonly ToDoEventService _service;
    private readonly CalendarEventService _calendarEventService;

    public EventController(ToDoEventService service, CalendarEventService calendarEventService)
    {
        _service = service;
        _calendarEventService = calendarEventService;
    }

    [HttpGet("{id}")]
    public IEnumerable<ToDoEvent> GetForCalendarItem([FromRoute] string id)
    {
        return _service.GetAll(id).OrderByDescending(s => s.Date);
    }

    [HttpGet("{calendarItemUid}/new")]
    public ToDoEvent CreateNew([FromRoute] string calendarItemUid)
    {
        var nextId = _service.GetAll(calendarItemUid).Count() + 1;

        return new ToDoEvent
        {
            CalendarToDoId = calendarItemUid,
            Date = DateTime.Today,
            EventId = nextId,
            Remarks = "",
            AdjustCalendar = false,
        };
    }

    [HttpDelete("{calendarEventId}/{id}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public IActionResult Delete([FromRoute] string calendarEventId, int id)
    {
        var item = _service.Get(calendarEventId, id);

        if (item == null)
        {
            return NotFound();
        }

        _service.Delete(item);

        return Ok();
    }

    [HttpPost()]
    public ToDoEvent Save([FromBody] ToDoEvent item)
    {
        _service.Save(item);

        if (item.AdjustCalendar.HasValue && item.AdjustCalendar.Value)
        {
            Update(_calendarEventService.GetEnabled(item.CalendarToDoId), true);
            Update(_calendarEventService.GetDisabled(item.CalendarToDoId), false);
        }

        return item;
    }

    private void Update(CalendarEvent? @event, bool isEnabled)
    {
        if (@event == null)
        {
            return;
        }

        @event.Start = new CalDateTime(DateTime.Today);
        _calendarEventService.Save(@event, isEnabled);
    }
}
