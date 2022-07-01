using CalDo.Functions;
using CalDo.Models;
using Microsoft.AspNetCore.Mvc;

namespace CalDo.Controllers;

[ApiController]
[Route("api/todo")]
public class ToDoController : ControllerBase
{
    private readonly PersistenceService _service;

    public ToDoController(PersistenceService service)
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
    public ToDoVM Get([FromRoute] int id)
    {
        return ToDoVM.From(_service.Get(id));
    }

    [HttpPost()]
    public ToDoVM Save([FromBody] ToDoVM item)
    {
        _service.Save(ToDoVM.ToCalendarEvent(item), item.Enabled);
        return item;
    }
}
