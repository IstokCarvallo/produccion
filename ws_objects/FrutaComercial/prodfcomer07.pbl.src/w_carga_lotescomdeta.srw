$PBExportHeader$w_carga_lotescomdeta.srw
forward
global type w_carga_lotescomdeta from w_mant_directo
end type
type dw_2 from datawindow within w_carga_lotescomdeta
end type
type st_1 from statictext within w_carga_lotescomdeta
end type
type sle_archivo from singlelineedit within w_carga_lotescomdeta
end type
end forward

global type w_carga_lotescomdeta from w_mant_directo
integer width = 4087
string title = "CARGA DE ARCHIVO PLANO DE LOTES COMERCIALES"
dw_2 dw_2
st_1 st_1
sle_archivo sle_archivo
end type
global w_carga_lotescomdeta w_carga_lotescomdeta

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

DELETE dba.spro_lotesfrutacomdeta_archivo
 WHERE lfcd_archiv = :is_archivo;

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

on w_carga_lotescomdeta.create
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

on w_carga_lotescomdeta.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.st_1)
destroy(this.sle_archivo)
end on

event open;call super::open;pb_lectura.TriggerEvent("clicked")
dw_2.SetTransObject(SQLCa)
end event

type st_encabe from w_mant_directo`st_encabe within w_carga_lotescomdeta
integer width = 3346
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_carga_lotescomdeta
integer x = 3520
long backcolor = 553648127
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

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_lectura from w_mant_directo`pb_lectura within w_carga_lotescomdeta
boolean visible = false
integer x = 3520
long backcolor = 553648127
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_carga_lotescomdeta
boolean visible = false
integer x = 3520
long backcolor = 553648127
end type

type pb_insertar from w_mant_directo`pb_insertar within w_carga_lotescomdeta
string tag = "Carga Archivo Plano"
integer x = 3520
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
long backcolor = 553648127
end type

event pb_insertar::clicked;String	ls_archivo, ls_data, ls_columna, ls_archivos[]
Integer	li_fila, li_nuevo
String	ls_cuenta, ls_nulo

SetNull(ls_nulo)

li_fila	=	GetFileOpenName("Seleccione archivo para cargar", &
									ls_archivo, ls_archivos, "CSV", &
									"Comma Separated Values (*.CSV),*.CSV,"+&
									"Tab Separated Values (*.TXT),*.TXT", GetCurrentDirectory(), 18)

li_fila				=	dw_1.ImportFile(ls_archivo)

is_archivo			=	ls_archivos[1]
sle_archivo.Text	=	ls_archivos[1]

IF dw_1.RowCount() > 1 THEN
	FOR li_fila = 1 TO dw_1.RowCount()
		IF dw_1.Object.lfcd_secuen[li_fila] > 0 AND Not IsNull(dw_1.Object.lfcd_secuen[li_fila]) THEN
			dw_1.Object.lfcd_archiv[li_fila]	=	is_archivo
			dw_1.Object.lofc_pltcod[li_fila]	=	gstr_ParamPlanta.CodigoPlanta
		END IF
	NEXT
END IF

Parent.TriggerEvent("ue_guardar")

dw_2.Retrieve(ls_archivos)

end event

type pb_salir from w_mant_directo`pb_salir within w_carga_lotescomdeta
integer x = 3520
long backcolor = 553648127
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_carga_lotescomdeta
boolean visible = false
integer x = 3520
long backcolor = 553648127
end type

type pb_grabar from w_mant_directo`pb_grabar within w_carga_lotescomdeta
string tag = "Aprobación Pallet Archivo"
boolean visible = false
integer x = 3520
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

type dw_1 from w_mant_directo`dw_1 within w_carga_lotescomdeta
boolean visible = false
integer y = 1716
integer width = 3346
integer height = 140
string dataobject = "dw_mues_spro_lotesfrutacomdeta_archivo"
end type

type dw_2 from datawindow within w_carga_lotescomdeta
integer x = 78
integer y = 268
integer width = 3346
integer height = 1440
integer taborder = 20
boolean titlebar = true
string title = "Cajas En Archivo"
string dataobject = "dw_mues_spro_lotesfrutacomdeta_grupo"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_carga_lotescomdeta
integer x = 357
integer y = 120
integer width = 279
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Archivo"
boolean focusrectangle = false
end type

type sle_archivo from singlelineedit within w_carga_lotescomdeta
integer x = 663
integer y = 108
integer width = 2528
integer height = 100
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

