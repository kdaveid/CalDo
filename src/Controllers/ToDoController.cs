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

    [HttpGet("create", Name = "CreateNew")]
    public ToDoVM New()
    {
        return new ToDoVM { Uid = Guid.NewGuid().ToString(), StartDT = DateTime.Now, EndDT = DateTime.Now.AddHours(1) };
    }

    [HttpGet(Name = "GetAll")]
    public IEnumerable<ToDoVM> Get()
    {
        return _service.GetAll().Select(s => ToDoVM.From(s));
    }

    [HttpGet("{id}")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ToDoVM))]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public IActionResult Get([FromRoute] string id)
    {
        var item = _service.GetEnabled(id);
        if (item != null)
        {
            return Ok(ToDoVM.From(item));
        }

        item = _service.GetDisabled(id);
        if (item != null)
        {
            return Ok(ToDoVM.From(item));
        }

        return NotFound();
    }

    [HttpPost()]
    public ToDoVM Save([FromBody] ToDoVM item)
    {
        _service.Save(ToDoVM.ToCalendarEvent(item), item.Enabled);
        return item;
    }
}
