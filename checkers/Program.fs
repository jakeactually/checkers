namespace checkers
#nowarn "20"
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddControllers()
        builder.Services.AddMvc().AddSessionStateTempDataProvider()
        builder.Services.AddSession()
        builder.Services.AddHttpContextAccessor()

        let app = builder.Build()

        app.UseHttpsRedirection()

        app.UseAuthorization()
        app.UseSession()
        app.UseWebSockets()
        app.MapControllers()

        app.Run()

        exitCode
