$PBExportHeader$w_consulta_lote.srw
$PBExportComments$Consulta por lote entregando su detalle
forward
global type w_consulta_lote from w_mant_directo
end type
type dw_2 from datawindow within w_consulta_lote
end type
type dw_3 from datawindow within w_consulta_lote
end type
end forward

global type w_consulta_lote from w_mant_directo
integer width = 3909
string title = "Consulta del Lote"
dw_2 dw_2
dw_3 dw_3
end type
global w_consulta_lote w_consulta_lote

type variables
DataWindowChild				idwc_variedades
end variables

forward prototypes
public subroutine buscalote ()
end prototypes

public subroutine buscalote ();String  ls_Lote, ls_Null
Str_busqueda	lstr_busq

SetNull(ls_Null)

lstr_busq.Argum[1]	= String(gstr_Paramplanta.codigoplanta)
lstr_busq.Argum[2]	= ""
lstr_busq.Argum[3]	= "Consulta"

OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_3.SetColumn("lote")
	dw_3.SetFocus()
ELSE

	dw_3.Object.lote_pltcod[1]	=	Integer(lstr_busq.argum[1])
	dw_3.Object.lote_espcod[1]	=	Integer(lstr_busq.argum[2])
	dw_3.Object.lote_codigo[1]	=	Integer(lstr_busq.argum[3])
	
	Istr_Mant.Argumento[1]	=	lstr_busq.argum[1]
	Istr_Mant.Argumento[2]	=	lstr_busq.argum[2]
	Istr_Mant.Argumento[3]	=	lstr_busq.argum[3]

	Istr_Mant.Argumento[4]			=	String(dw_3.Object.lote_pltcod[1],"0000") + &
												String(dw_3.Object.lote_espcod[1],"00") + &
												String(dw_3.Object.lote_codigo[1],"0000")

	dw_3.Object.lote[1] = Istr_Mant.Argumento[4]
	TriggerEvent("ue_recuperadatos")
	dw_3.SetColumn("lote")
	dw_3.SetFocus()
END IF

RETURN

end subroutine

on w_consulta_lote.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_consulta_lote.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event open;call super::open;

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_3.InsertRow(0)

pb_lectura.enabled = False

dw_3.SetColumn("lote")
dw_3.SetFocus()




end event

event closequery;//
end event

event ue_recuperadatos();call super::ue_recuperadatos;String ls_Null
Integer li_fila		

SetNull(ls_Null)

li_fila = dw_3.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
  					  		  Integer(istr_Mant.Argumento[3]))
pb_lectura.enabled = true
IF li_Fila <= 0 THEN
	dw_3.InsertRow(0)
	Return	
ELSEIF li_fila > 0 THEN
	dw_3.Object.lote[1] = Istr_Mant.Argumento[4]
END IF
end event

event resize;call super::resize;dw_2.x		=	dw_1.x
dw_3.x		=	dw_1.x
dw_2.Width	=	dw_1.Width
dw_3.Width	=	dw_1.Width
end event

type st_encabe from w_mant_directo`st_encabe within w_consulta_lote
boolean visible = false
integer x = 2921
integer y = 588
integer width = 229
integer height = 492
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_consulta_lote
integer x = 3296
integer y = 584
end type

event pb_nuevo::clicked;call super::clicked;dw_2.Reset()
dw_3.Reset()
dw_3.Insertrow(0)

dw_3.Object.lote.protect = 0
dw_3.SetColumn("lote")
pb_lectura.Enabled = True
end event

type pb_lectura from w_mant_directo`pb_lectura within w_consulta_lote
integer x = 3314
integer y = 136
end type

event pb_lectura::clicked;Integer li_Planta, li_Especie, li_folio, li_PlantaBusq

IF dw_3.Object.todasplantas[1] = 0 THEN
	li_PlantaBusq	=	gstr_Paramplanta.codigoplanta
END IF
	
IF dw_1.Retrieve(li_PlantaBusq,Integer(istr_Mant.Argumento[1]),&
					  Integer(istr_Mant.Argumento[2]),Integer(istr_Mant.Argumento[3])) >= 0  OR &
	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
                 Integer(istr_Mant.Argumento[3])) >= 0  THEN			
	pb_lectura.enabled = false
	dw_3.Object.lote.Protect = 1
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF

RETURN
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_consulta_lote
boolean visible = false
integer x = 3387
integer y = 1152
end type

type pb_insertar from w_mant_directo`pb_insertar within w_consulta_lote
boolean visible = false
integer x = 3365
integer y = 1112
end type

type pb_salir from w_mant_directo`pb_salir within w_consulta_lote
integer x = 3314
integer y = 1536
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_consulta_lote
boolean visible = false
integer x = 3291
integer y = 760
end type

type pb_grabar from w_mant_directo`pb_grabar within w_consulta_lote
boolean visible = false
integer x = 3383
integer y = 1112
end type

type dw_1 from w_mant_directo`dw_1 within w_consulta_lote
integer x = 101
integer y = 1108
integer width = 3035
integer height = 720
boolean titlebar = true
string title = "Detalle de Existencia"
string dataobject = "dw_consulta_lotes"
end type

type dw_2 from datawindow within w_consulta_lote
integer x = 101
integer y = 468
integer width = 3035
integer height = 628
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Recepción del Lote"
string dataobject = "dw_consulta_spro_lotesfrutagrandeta"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_consulta_lote
integer x = 101
integer y = 20
integer width = 3035
integer height = 380
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_consulta_lotes_seleccion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String ls_columna, ls_Null
Integer li_fila

Setnull(ls_Null)
dw_3.AcceptText()
ls_Columna = dwo.Name
Istr_Mant.Argumento[4] = Data

CHOOSE CASE ls_Columna
	CASE "lote"
      IF len(data) = 10 THEN
			istr_Mant.Argumento[1]	=	Mid(data,1,4)
			istr_Mant.Argumento[2]	=	Mid(data,5,2)
			istr_Mant.Argumento[3]	=	Mid(data,7,4)
			li_fila = dw_3.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]), &
						  		 Integer(istr_Mant.Argumento[3]))
			pb_lectura.enabled = true
			IF li_Fila <= 0 THEN
				dw_3.InsertRow(0)
				MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
				dw_3.Object.lote[1] = ls_Null
				Return
			ELSE
				dw_3.Object.lote[1] = Istr_Mant.Argumento[4]
				dw_3.Object.lote.Protect = 1
			END IF
		ELSE
			MessageBox("Atención","La cantidad de digitos Ingresados no es Valida",Exclamation!,Ok!)
			dw_3.Object.lote[1] = ls_Null
			Return	
		END IF
END CHOOSE	
	
	
	
	
	
	
	
	
	
	
	
	
	

end event

event buttonclicked;CHOOSE CASE dwo.Name
	CASE "busc_lote"
		pb_nuevo.TriggerEvent("Clicked")
		buscalote()

END CHOOSE
end event

