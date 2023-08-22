$PBExportHeader$w_proc_actualiza_produccion.srw
$PBExportComments$Ventana para actualización de producción a partir de log interpretado.
forward
global type w_proc_actualiza_produccion from window
end type
type mle_1 from multilineedit within w_proc_actualiza_produccion
end type
type dw_2 from datawindow within w_proc_actualiza_produccion
end type
type sle_archivo from singlelineedit within w_proc_actualiza_produccion
end type
type st_2 from statictext within w_proc_actualiza_produccion
end type
type st_1 from statictext within w_proc_actualiza_produccion
end type
type pb_imprimir from picturebutton within w_proc_actualiza_produccion
end type
type pb_archivo from picturebutton within w_proc_actualiza_produccion
end type
type pb_salir from picturebutton within w_proc_actualiza_produccion
end type
type pb_grabar from picturebutton within w_proc_actualiza_produccion
end type
type gb_1 from groupbox within w_proc_actualiza_produccion
end type
end forward

global type w_proc_actualiza_produccion from window
integer width = 3598
integer height = 1772
boolean titlebar = true
string title = "Carga Pallet desde Producción"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 30586022
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion ( )
event ue_imprimir pbm_custom03
mle_1 mle_1
dw_2 dw_2
sle_archivo sle_archivo
st_2 st_2
st_1 st_1
pb_imprimir pb_imprimir
pb_archivo pb_archivo
pb_salir pb_salir
pb_grabar pb_grabar
gb_1 gb_1
end type
global w_proc_actualiza_produccion w_proc_actualiza_produccion

type variables
protected:
Long		il_fila
Boolean	ib_datos_ok, ib_borrar, ib_ok
Date		id_FechaAcceso
Time		it_HoraAcceso

Menu		im_menu

Str_parms	istr_parms
Str_info		istr_info
end variables

event ue_guardar();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

String		ls_SentenciaSql, ls_Errores, ls_Registro
Integer		li_Archivo, li_Retorno

//ls_SentenciaSql	=	"DBTOOL TRANSLATE LOG FROM '" + sle_archivo.text + &
//							"' TO 'G:\Bases\CargaLog\Traspaso.sql'"
//EXECUTE IMMEDIATE :ls_SentenciaSql ;
//
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Ejecución de Sentencia")
ELSE
//	li_Archivo	= FileOpen("G:\Bases\CargaLog\Traspaso.sql")
	li_Archivo	= FileOpen(sle_archivo.text)

	IF li_Archivo < 0 THEN
		li_Retorno				=	MessageBox("Error", "Archivo no pudo ser abierto.", &
														Exclamation!, RetryCancel!)
		Message.DoubleParm	=	li_Retorno
	ELSE
		SetPointer(HourGlass!)
	
		DO WHILE FileRead(li_Archivo, ls_Registro) >= 0
			IF Len(ls_Registro) > 1 THEN
				IF Mid(ls_Registro, 1, 2) = "go" THEN
					mle_1.Text	=	ls_SentenciaSql
					
					EXECUTE IMMEDIATE :ls_SentenciaSql ;
					
					IF sqlca.SQLCode = -1 THEN
						F_ErrorBaseDatos(sqlca, "Ejecución de Sentencia : ~r" + ls_SentenciaSql)
					END IF
					
					ls_SentenciaSql	=	""
				ELSEIF    Mid(ls_Registro, 1, 1) <> "-" AND &
							 Mid(ls_Registro, 1, 1) <> "%" AND &
							 Pos(ls_Registro, "SETUSER") = 0 AND &
							(Pos(ls_Registro, "PLANTADESP") <> 0) OR &							 
							(Pos(ls_Registro, "PRODUCTORES") <> 0) OR &
							(Pos(ls_Registro, "VARIEDADES") <> 0) OR &
							(Pos(ls_Registro, "VARIECALIBRE") <> 0) OR &							
							(Pos(ls_Registro, "PUERTOS") <> 0) OR &
							(Pos(ls_Registro, "DESTINOS") <> 0) OR &
							(Pos(ls_Registro, "ENVASES") <> 0) OR &
							(Pos(ls_Registro, "EMBALAJES") <> 0) OR &							
							(Pos(ls_Registro, "RECIBIDORES") <> 0) THEN
							ls_SentenciaSql	=	""
					ELSEIF Mid(ls_Registro, 1, 1) <> "-" AND &
							Mid(ls_Registro, 1, 1) <> "%" AND &
							Pos(ls_Registro, "SETUSER") = 0 THEN
							ls_SentenciaSql	+=	ls_Registro
				END IF
				
				ls_Errores		= 	''
			END IF
		LOOP	
		
		FileClose(li_Archivo)
		
		SetPointer(Arrow!)
		
		Message.DoubleParm = li_Retorno
	
	
		DECLARE Actualizacion PROCEDURE FOR dba.FProc_FechaUltimaActualiza;
			
		EXECUTE Actualizacion; 
	
		SetPointer(Arrow!)
	
		IF SQLCA.SQLCode < 0 THEN
			F_ErrorBaseDatos(sqlca, "Actualización de Fechas.")
		ELSE
			Close Actualizacion;
	
		END IF
	END IF
END IF
end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event ue_seleccion();String	ls_directorio, ls_archivo
Integer	li_valida, li_opcion = 1
dwitemstatus stat

ib_ok	= True

IF li_Valida > 0 THEN 
	FileClose(li_valida)
END IF

DO
	ls_directorio	=	"G:\Bases"
	ls_archivo		=	"G:\Bases"
	li_valida		=	GetFileOpenName("Carga de Existencias", ls_directorio, ls_archivo, &
												"", "Transacciones (*.sql), *.sql, Todos los " + &
												"Archivos (*.*), *.*")
//	li_valida		=	GetFileOpenName("Carga de Existencias", ls_directorio, ls_archivo, &
//												"", "Transacciones (*.log), *.log, Todos los " + &
//												"Archivos (*.*), *.*")
	IF li_valida = 0 THEN
		pb_salir.SetFocus()
		RETURN
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE
		ls_archivo			=	ls_directorio
		sle_archivo.text	=	ls_archivo

		Message.DoubleParm	= 2
		
		pb_grabar.Enabled		=	True
	END IF
	
LOOP WHILE Message.DoubleParm = 1
end event

event open;x	=0
y	=0
This.Width	= mle_1.width + 540
This.Height	= 1993
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

This.ParentWindow().ToolBarVisible	=	True

pb_archivo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)


end event

on w_proc_actualiza_produccion.create
this.mle_1=create mle_1
this.dw_2=create dw_2
this.sle_archivo=create sle_archivo
this.st_2=create st_2
this.st_1=create st_1
this.pb_imprimir=create pb_imprimir
this.pb_archivo=create pb_archivo
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_1=create gb_1
this.Control[]={this.mle_1,&
this.dw_2,&
this.sle_archivo,&
this.st_2,&
this.st_1,&
this.pb_imprimir,&
this.pb_archivo,&
this.pb_salir,&
this.pb_grabar,&
this.gb_1}
end on

on w_proc_actualiza_produccion.destroy
destroy(this.mle_1)
destroy(this.dw_2)
destroy(this.sle_archivo)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_imprimir)
destroy(this.pb_archivo)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_1)
end on

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= This.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF

IF li_vta = 1 THEN
	This.ParentWindow().ToolBarVisible	= False
	im_menu.Item[1].Item[6].Enabled		= False
	im_menu.Item[7].Visible					= False
	
END IF

GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

event resize;Integer	maximo

maximo	= mle_1.width

mle_1.x					= 78
mle_1.y					= 321
mle_1.height			= This.WorkSpaceHeight() - mle_1.y - 41
gb_1.width				= 275
gb_1.height				= 817
gb_1.x 					= This.WorkSpaceWidth() - 351
gb_1.y 					= 493
pb_archivo.x			= This.WorkSpaceWidth() - 292
pb_archivo.y			= 577
pb_archivo.width		= 156
pb_archivo.height		= 133
pb_grabar.x				= pb_archivo.x
pb_grabar.y				= 757
pb_grabar.width		= 156
pb_grabar.height		= 133
pb_imprimir.x			= pb_archivo.x
pb_imprimir.y			= 937
pb_imprimir.width		= 156
pb_imprimir.height	= 133
pb_salir.x				= pb_archivo.x
pb_salir.y				= 1117
pb_salir.width			= 156
pb_salir.height		= 133
end event

type mle_1 from multilineedit within w_proc_actualiza_produccion
integer x = 78
integer y = 336
integer width = 3063
integer height = 1264
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_proc_actualiza_produccion
boolean visible = false
integer x = 78
integer y = 1204
integer width = 3063
integer height = 324
integer taborder = 41
string dataobject = "dw_mues_palletencab"
boolean hscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

type sle_archivo from singlelineedit within w_proc_actualiza_produccion
integer x = 713
integer y = 116
integer width = 2368
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_proc_actualiza_produccion
integer x = 151
integer y = 124
integer width = 517
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Archivo de Carga"
boolean focusrectangle = false
end type

type st_1 from statictext within w_proc_actualiza_produccion
integer x = 78
integer y = 68
integer width = 3063
integer height = 188
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_proc_actualiza_produccion
event clicked pbm_bnclicked
integer x = 3223
integer y = 1124
integer width = 233
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\ImprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\ImprimirDisab.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_imprimir")
end event

type pb_archivo from picturebutton within w_proc_actualiza_produccion
integer x = 3223
integer y = 776
integer width = 233
integer height = 196
integer taborder = 10
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscaArch.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_seleccion")
end event

type pb_salir from picturebutton within w_proc_actualiza_produccion
integer x = 3223
integer y = 1304
integer width = 233
integer height = 196
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_proc_actualiza_produccion
integer x = 3223
integer y = 944
integer width = 233
integer height = 196
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_1 from groupbox within w_proc_actualiza_produccion
boolean visible = false
integer x = 3205
integer y = 708
integer width = 274
integer height = 816
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

