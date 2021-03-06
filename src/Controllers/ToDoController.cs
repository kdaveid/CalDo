using System.Net;
using CalDo.Functions;
using CalDo.Models;
using Microsoft.AspNetCore.Mvc;

namespace CalDo.Controllers;

[ApiController]
[Route("api/todo")]
public class ToDoController : ControllerBase
{
    private readonly CalendarEventService _service;

    public ToDoController(CalendarEventService service)
    {
        _service = service;
    }

    [HttpGet("create")]
    public ToDoVM New()
    {
        return new ToDoVM
        {
            Uid = Guid.NewGuid().ToString(),
            Name = "new",
            StartDT = DateTime.Now,
            EndDT = DateTime.Now.AddHours(1),
            Interval = 1,
            Frequency = FrequencyType.Monthly.ToString(),
            Alarm = AlarmVM.DisabledAlarm
        };
    }

    [HttpGet(Name = "GetAll")]
    public IEnumerable<ToDoVM> Get()
    {
        return _service.GetAll();
    }

    [HttpGet("{id}")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ToDoVM))]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public IActionResult Get([FromRoute] string id)
    {
        var item = _service.GetEnabled(id);
        if (item != null)
        {
            return Ok(ToDoVM.From(item, true));
        }

        item = _service.GetDisabled(id);
        if (item != null)
        {
            return Ok(ToDoVM.From(item, false));
        }

        return NotFound();
    }

    [HttpPost()]
    public ToDoVM Save([FromBody] ToDoVM item)
    {
        _service.Save(ToDoVM.ToCalendarEvent(item), item.Enabled);
        return item;
    }

    [HttpPost("delete")]
    [ProducesResponseType(typeof(ToDoVM), (int)HttpStatusCode.OK)]
    [ProducesResponseType(typeof(ToDoVM), (int)HttpStatusCode.NotFound)]
    public IActionResult Delete([FromBody] ToDoVM item)
    {
        if (item.Uid == null)
        {
            return NotFound(item);
        }

        _service.Delete(item.Uid);
        return Ok(item);
    }
}
