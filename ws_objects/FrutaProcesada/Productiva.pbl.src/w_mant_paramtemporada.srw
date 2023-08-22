$PBExportHeader$w_mant_paramtemporada.srw
forward
global type w_mant_paramtemporada from window
end type
type pb_3 from picturebutton within w_mant_paramtemporada
end type
type pb_2 from picturebutton within w_mant_paramtemporada
end type
type dw_1 from datawindow within w_mant_paramtemporada
end type
type gb_1 from groupbox within w_mant_paramtemporada
end type
end forward

global type w_mant_paramtemporada from window
integer x = 46
integer y = 48
integer width = 3470
integer height = 1996
boolean titlebar = true
string title = "Mantención Parametros de Comercio Exterior"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 30586022
event ue_antesguardar pbm_custom75
event ue_guardar pbm_custom11
event ue_recuperadatos pbm_custom15
event ue_imprimir pbm_custom03
pb_3 pb_3
pb_2 pb_2
dw_1 dw_1
gb_1 gb_1
end type
global w_mant_paramtemporada w_mant_paramtemporada

type variables
String	is_rutrl1,is_rutrl2

end variables

forward prototypes
protected function boolean wf_actualiza_db ()
end prototypes

event ue_antesguardar;Long	ll_fila = 1

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Long		fila
//str_info	lstr_info
//
//lstr_info.titulo	= "PARÁMETROS DE COMERCIO EXTERIOR"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_paramtemporada"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve()
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No Existe información para este informe.", &
//					StopSign!, OK!)
//	
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//	
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
//					
end event

protected function boolean wf_actualiza_db ();if dw_1.update() = 1 then 
	commit;
	if sqlca.sqlcode <> 0 then
		F_ErrorBaseDatos(sqlca,this.title)
		return false
	else
		return true
	end if 
else
	rollback;
	if sqlca.sqlcode <> 0 then F_ErrorBaseDatos(sqlca,this.title)
	return false
end if
return true
end function

on w_mant_paramtemporada.create
this.pb_3=create pb_3
this.pb_2=create pb_2
this.dw_1=create dw_1
this.gb_1=create gb_1
this.Control[]={this.pb_3,&
this.pb_2,&
this.dw_1,&
this.gb_1}
end on

on w_mant_paramtemporada.destroy
destroy(this.pb_3)
destroy(this.pb_2)
destroy(this.dw_1)
destroy(this.gb_1)
end on

event open;Long ll_fila

x				= 0
y				= 0

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

ll_fila = dw_1.Retrieve()

IF ll_fila = 0 THEN
	dw_1.InsertRow(0)
END IF	


								

end event

type pb_3 from picturebutton within w_mant_paramtemporada
event ue_mousemove pbm_mousemove
integer x = 3173
integer y = 420
integer width = 233
integer height = 196
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Grabar actual información")
end event

event clicked;Parent.TriggerEvent("ue_guardar")

end event

type pb_2 from picturebutton within w_mant_paramtemporada
event ue_mousemove pbm_mousemove
integer x = 3173
integer y = 772
integer width = 233
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Salir de Parametros")
end event

event clicked;Close(Parent)
end event

type dw_1 from datawindow within w_mant_paramtemporada
integer x = 18
integer y = 32
integer width = 2912
integer height = 1800
integer taborder = 10
string dataobject = "dw_mant_paramtemporada"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Null

SetNull(ls_Null)

CHOOSE CASE dwo.Name
			
	CASE "pate_rurec1"
		
		is_rutrl1 = F_verrut(data, True)
		
		IF is_rutrl1 = ""  THEN
			This.SetItem(1, "pate_rurec1", ls_Null)
			Return 1
		END IF
		
	CASE "pate_rurec2"
		
		is_rutrl2 = F_verrut(data, True)
		
		IF is_rutrl2 = ""  THEN
			This.SetItem(1, "pate_rurec2", ls_Null)
			RETURN 1
		END IF
		
	
END CHOOSE


end event

event itemerror;Return 1
end event

event itemfocuschanged;	IF is_rutrl1 <> "" THEN
		IF dwo.Name = "pate_rurec1" THEN
			IF is_rutrl1 <> "" THEN
				This.SetItem(1, "pate_rurec1", String(Double(Mid(is_rutrl1, 1, 9)), "#########") + Mid(is_rutrl1, 10))
			END IF
		ELSE
			This.SetItem(1, "pate_rurec1", is_rutrl1)
		END IF
	END IF
	
	IF is_rutrl2 <> "" THEN
		IF dwo.Name = "pate_rurec2" THEN
			IF is_rutrl2 <> "" THEN
				This.SetItem(1, "pate_rurec2", String(Double(Mid(is_rutrl2, 1, 9)), "#########") + Mid(is_rutrl2, 10))
			END IF
		ELSE
			This.SetItem(1, "pate_rurec2", is_rutrl2)
		END IF
	END IF
	

end event

type gb_1 from groupbox within w_mant_paramtemporada
boolean visible = false
integer x = 3131
integer y = 204
integer width = 279
integer height = 952
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 30586022
end type

