$PBExportHeader$w_carga_cajasprod.srw
forward
global type w_carga_cajasprod from w_mant_directo
end type
type dw_2 from datawindow within w_carga_cajasprod
end type
type st_1 from statictext within w_carga_cajasprod
end type
type sle_archivo from singlelineedit within w_carga_cajasprod
end type
end forward

global type w_carga_cajasprod from w_mant_directo
integer width = 2894
string title = "CARGA DE CAJAS PACKING"
dw_2 dw_2
st_1 st_1
sle_archivo sle_archivo
end type
global w_carga_cajasprod w_carga_cajasprod

type variables
String	is_archivo
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

DELETE dba.spro_cajasprod_carga
 WHERE capr_nomarc = :is_archivo;

IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
	Rollback;
ELSE
	IF dw_1.Update(True, False) = 1 then 
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		ELSE
			lb_Retorno	=	True
				
			dw_1.Reset()
		END IF
	ELSE
		RollBack;
		
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		
		lb_Retorno	=	False
	END IF
END IF
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno

end function

on w_carga_cajasprod.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.st_1=create st_1
this.sle_archivo=create sle_archivo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.sle_archivo
end on

on w_carga_cajasprod.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.st_1)
destroy(this.sle_archivo)
end on

event resize;Integer		li_posi_y, li_objeto

dw_2.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_2.y - 75)

dw_2.x					= 	78
st_encabe.width		= 	dw_2.width

pb_lectura.x			= 	This.WorkSpaceWidth() - 292
pb_lectura.y			= 	300
pb_lectura.width		=	156
pb_lectura.height		= 	133

pb_nuevo.x				= 	This.WorkSpaceWidth() - 292
pb_nuevo.width			= 	156
pb_nuevo.height		= 	133

pb_insertar.x			= 	This.WorkSpaceWidth() - 292
pb_insertar.width		= 	156
pb_insertar.height	= 	133

pb_eliminar.x			= 	This.WorkSpaceWidth() - 292
pb_eliminar.width		= 	156
pb_eliminar.height	= 	133

pb_grabar.x				= 	This.WorkSpaceWidth() - 292
pb_grabar.width		= 	156
pb_grabar.height		= 	133

pb_imprimir.x			= 	This.WorkSpaceWidth() - 292
pb_imprimir.width		= 	156
pb_imprimir.height	= 	133

li_posi_y				= 	1300

IF pb_nuevo.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_nuevo.y	= li_posi_y
END IF

IF pb_insertar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_insertar.y	= li_posi_y
END IF

IF pb_eliminar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_eliminar.y	= li_posi_y
END IF

IF pb_grabar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_grabar.y		= li_posi_y
END IF

IF pb_imprimir.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_imprimir.y	= li_posi_y
END IF

pb_salir.x			= 	This.WorkSpaceWidth() - 292
pb_salir.y			= 	1300+ 88
pb_salir.width		= 	156
pb_salir.height	=	133
end event

event open;call super::open;pb_lectura.TriggerEvent("clicked")
dw_2.SetTransObject(SQLCa)
end event

event ue_nuevo;call super::ue_nuevo;dw_2.Reset()
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "INFORME REVISION CARGA DE ARCHIVO PLANO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_cajas_carga_resumen"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(is_archivo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_carga_cajasprod
integer width = 2377
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_carga_cajasprod
integer x = 2592
end type

event pb_nuevo::clicked;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE 0
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					Parent.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN RETURN
					
				CASE 3
					RETURN
			END CHOOSE
	END CHOOSE
END IF

wf_BloqueaColumnas(False)

dw_1.Reset()
dw_2.Reset()
sle_archivo.Text	=	''

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_lectura from w_mant_directo`pb_lectura within w_carga_cajasprod
boolean visible = false
integer x = 2592
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_carga_cajasprod
boolean visible = false
integer x = 2592
end type

type pb_insertar from w_mant_directo`pb_insertar within w_carga_cajasprod
string tag = "Carga Archivo Plano"
integer x = 2592
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo\Bmp\Apuntee.bmp"
string disabledname = "\Desarrollo\Bmp\Apunted.bmp"
end type

event pb_insertar::clicked;String	ls_archivo, ls_data, ls_columna, ls_archivos[]
Integer	li_fila, li_nuevo
String	ls_cuenta, ls_nulo

SetNull(ls_nulo)

//SetNull(ls_archivo)  

li_fila	=	GetFileOpenName("Seleccione archivo para cargar", &
									ls_archivo, ls_archivos, "CSV", &
									"Comma Separated Values (*.CSV),*.CSV", GetCurrentDirectory(), 18)

IF li_fila < 1 THEN RETURN

dw_1.ImportFile(CSV!, ls_archivo)

is_archivo			=	ls_archivos[1]
sle_archivo.Text	=	ls_archivos[1]

IF dw_1.RowCount() > 0 THEN
	FOR li_fila = 1 TO dw_1.RowCount()
		dw_1.Object.capr_nomarc[li_fila]	=	is_archivo
		dw_1.Object.capr_docrel[li_fila]	=	Integer(ls_nulo)
		dw_1.Object.plde_codigo[li_fila]	=	gi_CodPlanta
	NEXT
END IF

Parent.TriggerEvent("ue_guardar")

dw_2.Retrieve(ls_archivos[1])

end event

type pb_salir from w_mant_directo`pb_salir within w_carga_cajasprod
integer x = 2592
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_carga_cajasprod
integer x = 2592
end type

type pb_grabar from w_mant_directo`pb_grabar within w_carga_cajasprod
string tag = "Aprobación Pallet Archivo"
boolean visible = false
integer x = 2592
string picturename = "\Desarrollo\Bmp\ACEPTAE.BMP"
string disabledname = "\Desarrollo\Bmp\ACEPTAD.BMP"
end type

type dw_1 from w_mant_directo`dw_1 within w_carga_cajasprod
boolean visible = false
integer y = 1716
integer width = 2377
integer height = 140
string dataobject = "dw_mues_cajas_carga"
end type

type dw_2 from datawindow within w_carga_cajasprod
integer x = 78
integer y = 268
integer width = 2377
integer height = 1440
integer taborder = 20
boolean titlebar = true
string title = "Cajas En Archivo"
string dataobject = "dw_mues_cajas_carga_resumen"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_carga_cajasprod
integer x = 357
integer y = 116
integer width = 279
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Archivo"
boolean focusrectangle = false
end type

type sle_archivo from singlelineedit within w_carga_cajasprod
integer x = 663
integer y = 108
integer width = 1559
integer height = 100
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

